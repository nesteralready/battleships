   
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Server where


import Control.Concurrent
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Hashable
import Data.List (delete, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Network.Socket
import Network.Socket.ByteString
import System.Exit (exitSuccess)
import System.Random
import Semaphore
import Data.ByteString.Lens ()
import Data.ByteString.Char8 as B8
import Data.Char(digitToInt)
import Board

--import qualified Data.ByteString.Char8 as B8

data User = User
    { _userID :: Int
    , _userNickName :: String
    , _userConn :: Socket
    }

data Channel = Channel
    { _channelID :: Int
    , _channelName :: String
    , _channelUsers :: [String]
    }

data GameState = GameState 
    { _myBoard :: String
    , _opponentBoard :: String
    , _turn :: Bool
    }

data ServerEnvr = ServerEnvr
    { _serverHost :: HostName
    , _serverPort :: String
    , _serverSock :: Socket
    , _serverSem :: Semaphore
    , _serverChannels :: MVar (Map Int Channel)
    , _serverUsers :: MVar (Map String User)
    }

makeLenses ''User
makeLenses ''Channel
makeLenses ''ServerEnvr

type Server = StateT ServerEnvr IO

maxConnections :: Int
maxConnections = 1

startServer :: String -> IO ()
startServer port =
    bracketOnError
        (do
            addrinfos <- getAddrInfo
                         (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                         Nothing (Just port)
            let serveraddr = Prelude.head addrinfos
            sock <- socket (addrFamily serveraddr) Stream defaultProtocol
            bind sock (addrAddress serveraddr)
            listen sock 1
            return sock)

        (\sock -> do
            Prelude.putStrLn "Terminating..."
            close sock)

        (\sock -> do
            Prelude.putStrLn $ "Listening on port " ++ port ++ "..."
            
            host <- getFQDN
            sem <- newSemaphore maxConnections
            channels <- newMVar Map.empty
            users <- newMVar Map.empty

            let env = ServerEnvr host port sock sem channels users
            acceptConnections env)

acceptConnections :: ServerEnvr -> IO ()
acceptConnections env = forever $ do
    let sock = env ^. serverSock
        sem  = env ^. serverSem

    accepted <- try (accept sock)
    case accepted of
        Left (_ :: IOException) -> exitSuccess
        Right (conn, _) -> do
            canAquireSem <- checkSemaphore sem
            if canAquireSem then do 
                void $ sendAll conn  "Opponents not connected"
                void $ forkIO $ void $ runStateT (processRequest conn) env
            else do

                void $ sendAll conn  "Game is Starting"
                void $ forkIO $ void $ runStateT (processRequest conn) env

processRequest :: Socket -> Server ()
processRequest conn = forever $ do
    request <- liftIO $ try (recv conn 4096)
    case request of
        Left (_ :: IOException) -> liftIO $ do
            Prelude.putStrLn "Client disconnected."
            exitSuccess
        Right msg -> do
            liftIO $ Prelude.putStrLn $ "RECEIVED REQUEST - " ++ B8.unpack msg
            handleRequest (B8.unpack msg) conn

handleRequest :: String -> Socket -> Server ()
handleRequest msg conn =
    let msgWords = Prelude.words msg in

    if "HELO" `Data.List.isPrefixOf` msg then
        handleHELO conn (msgWords !! 1)
    -- else if "(" `Data.List.isPrefixOf` msg then
    --     makeTurn 
    else if "SHOOT" `Data.List.isPrefixOf` msg then
        handleTurn conn (msgWords !! 1)
    else if "STOP_GAME" `Data.List.isPrefixOf` msg then
        use serverSock >>= liftIO . close
    else if "JOIN_CHATROOM" `Data.List.isPrefixOf` msg then
        handleJoin conn (parseParam $ Prelude.head msgWords) (parseParam $ msgWords !! 3)
    else if "LEAVE_CHATROOM" `Data.List.isPrefixOf` msg then
        handleLeave conn (read $ parseParam $ Prelude.head msgWords) (parseParam $ msgWords !! 1) (parseParam $ msgWords !! 2)
    else if "DISCONNECT" `Data.List.isPrefixOf` msg then
        handleDisconect conn (parseParam $ msgWords !! 2)
    else if "CHAT:" `Data.List.isPrefixOf` msg then
        handleChat (read $ parseParam $ Prelude.head msgWords) (parseParam $ msgWords !! 2) (parseParam $ msgWords !! 3)
    else
        handleUnknownRequest conn (msgWords !! 0)

parseParam :: String -> String
parseParam str = splitStr !! 1
    where splitStr = splitOn ":" str

sendResponse :: Socket -> String -> Server ()
sendResponse conn response = do
    liftIO $ Prelude.putStrLn $ ("SENDING RESPONSE - " ++ response ++ "\n")
    void $ liftIO $ sendAll conn (B8.pack response)

-- game state 
-- socket , gameState (myboard, opponentBoard,turn) 
handleTurn :: Socket -> String -> Server()
handleTurn sock coord =  do
    let response = "SHOOT AT COORDINATE (" ++ (showTuples (parseTurn coord)) ++ ")"

    sendResponse sock response

handleUnknownRequest :: Socket -> String ->Server()
handleUnknownRequest sock wrongComm= do
        let response = "unknown command, " ++ wrongComm ++"please make a correct turn informat 'SHOOT (x,y)' "
        sendResponse sock response

parseTurn :: String -> (Int, Int)
parseTurn (a:x:b:y:c) = (digitToInt x, digitToInt y)
parseTurn _ = (-1,-1)


showTuples :: (Int,Int) -> String
showTuples x = show(fst(x)) ++ " " ++ show(snd(x))

-- Joining
--Joining a chat room is initiated by a client by sending the following message to a chat server.
--JOIN_CHATROOM: [chatroom name]
--CLIENT_IP: [IP Address of client if UDP | 0 if TCP]
--PORT: [port number of client if UDP | 0 if TCP]
--CLIENT_NAME: [string Handle to identifier client user]

handleJoin :: Socket -> String -> String -> Server ()
handleJoin conn reqChannelName clientName = do
    usersMap <- getUsers
    case Map.lookup clientName usersMap of
        Nothing -> do
            joinID <- liftIO (randomIO :: IO Int)
            let newUser = User joinID clientName conn
            updateUsers $ Map.insert clientName newUser usersMap
        Just _ -> return ()

    channelsMap <- getChannels

    let channelHash = hash reqChannelName

    case Map.lookup channelHash channelsMap of
        Nothing -> do
            let newChannel = Channel channelHash reqChannelName [clientName]
            updateChannels $ Map.insert channelHash newChannel channelsMap
        Just channel -> do
            let newChannel = channel & channelUsers .~ clientName : (channel ^. channelUsers)
            updateChannels $ Map.adjust (const newChannel) channelHash channelsMap

    sendJoinResponse conn channelHash

sendJoinResponse :: Socket -> Int -> Server ()
sendJoinResponse conn channelHash = do
    channelsMap <- getChannels

    let channel = fromJust (channelsMap ^. at channelHash)
        chatroomName = channel ^. channelName
        roomRef = channel ^. channelID

    joinID <- liftIO (randomIO :: IO Int)
    serverIP <- use serverHost
    port <- use serverPort

    let response = "JOINED_CHATROOM:" ++ chatroomName ++ "\n" ++
                   "SERVER_IP:" ++ serverIP ++ "\n" ++
                   "PORT:" ++ port ++ "\n" ++
                   "ROOM_REF:" ++ show roomRef ++ "\n" ++
                   "JOIN_ID:" ++ show joinID

    sendResponse conn response

-- Leaving ----A client leaves a chat room by sending the following message to the chat server:
--LEAVE_CHATROOM: [ROOM_REF]
--JOIN_ID: [integer previously provided by server on join]
--CLIENT_NAME: [string Handle to identifier client user]

handleLeave :: Socket -> Int -> String -> String -> Server ()
handleLeave conn roomRef joinID clientName = do
    channelsMap <- getChannels
    case Map.lookup roomRef channelsMap of
        Just channel -> do
            let newChannel = channel & channelUsers .~ delete clientName (channel ^. channelUsers)
            updateChannels $ Map.adjust (const newChannel) roomRef channelsMap
        Nothing -> return ()

    sendLeaveResponse conn roomRef joinID

sendLeaveResponse :: Socket -> Int -> String -> Server ()
--The server responds with the following message:
sendLeaveResponse conn roomRef joinID = do
    let response = "LEFT_CHATROOM:" ++ show roomRef ++ "\n" ++
                   "JOIN_ID:" ++ joinID ++ "\n"

    sendResponse conn response

-- Disconnecting



handleDisconect :: Socket -> String -> Server ()
handleDisconect conn clientName = do
    usersMap <- getUsers
    updateUsers $ Map.delete clientName usersMap

    sendDisconnectResponse conn clientName

sendDisconnectResponse :: Socket -> String -> Server ()
sendDisconnectResponse conn clientName = do
    let response = "DISCONNECTED:" ++ clientName
    sendResponse conn response

-- Messaging
handleChat :: Int -> String -> String -> Server ()
handleChat roomRef clientName message = do
    usersMap <- getUsers
    channelsMap <- getChannels
    let users = fromJust (channelsMap ^. at roomRef) ^. channelUsers
    forM_ users $ \userName ->
        case Map.lookup userName usersMap of
            Nothing -> return ()
            Just user -> sendChatResponse (user ^. userConn) roomRef clientName message

sendChatResponse :: Socket -> Int -> String -> String -> Server ()
sendChatResponse conn roomRef clientName message = do
    let response = "CHAT:" ++ show roomRef ++ "\n" ++
					--"JOIN_ID:"++ show joinID ++ "\n" ++
                   "CLIENT_NAME:" ++ clientName ++ "\n" ++
                   "MESSAGE:" ++ message
    sendResponse conn response

-- HELOo

handleHELO :: Socket -> String -> Server ()
handleHELO conn message = do
    host <- use serverHost
    port <- use serverPort
    let response = message ++ "\n" ++
                   "IP:" ++ host ++ "\n" ++
                   "Port:" ++ port ++ "\n" ++
                   "StudentID: 17306521"
    sendResponse conn response

getChannels :: Server (Map Int Channel)
getChannels = use serverChannels >>= liftIO . readMVar

updateChannels :: Map Int Channel -> Server ()
updateChannels channels = do
    channelsMVar <- use serverChannels
    void $ liftIO $ swapMVar channelsMVar channels

getUsers :: Server (Map String User)
getUsers = use serverUsers >>= liftIO . readMVar

updateUsers :: Map String User -> Server ()
updateUsers users = do
    usersMVar <- use serverUsers
    void $ liftIO $ swapMVar usersMVar users

getFQDN :: IO HostName
getFQDN = do 
        return  "127.0.0.1"

--bind sock (SockAddrInet 3007 (tupleToHostAddress (127,0,0,1)))

