import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv, sendTo, sendAll)
import qualified Data.ByteString.Char8 as B8
import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (unless, forever)
import qualified Data.ByteString       as BS
import Game
import Board
-- only 3 ships
startGame::Socket -> String->IO()
startGame sock name = do 
            Game.start
            let ships = "(2,3)(3,4)(4,5) "
            sendAll sock $ B8.pack ("SHIPS " ++ ships)
            

startClient :: IO ()
startClient  = do
  print ("write your name to start the game")
  name <- getLine
 
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "290")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  sendAll sock $ B8.pack ("NAME " ++ name)
  playGame sock name


playGame::Socket ->String -> IO()
playGame sock  name=  do
    --sendAll sock $ B8.pack s
  msg <- recv sock 1024
  print ("TCP client received: " ++  B8.unpack msg)
  --case B8.unpack msg of
   --     "Game is Starting"  -> 
  onlineGameStarts sock name


onlineGameStarts::Socket -> String -> IO()
onlineGameStarts sock name = do startGame sock name


test :: IO ()
test  = forever $ do
  
 
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "23")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  msg <- recv sock 1024
  print ("TCP client received: " ++  B8.unpack msg)