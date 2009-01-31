import Network.Zephyr
import Control.Monad
import Control.Concurrent
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import System.IO

main :: IO ()
main = do initialize
          openPort
          cancelSubscriptions
          sender <- getSender
          realm  <- getRealm
          putStrLn $ "Sending as " ++ sender
          putStrLn $ "Sending from " ++ realm
          subscribeTo [ZSubscription (B.pack "nelhage-stress") (B.pack "*") (B.pack "*")]
          mapM_ (forkIO . recvLoop) [1..10]
          recvLoop 11

sendZephyrClass :: B.ByteString -> B.ByteString -> B.ByteString -> IO ()
sendZephyrClass cls inst body = sendNotice note
    where note = emptyNotice { z_kind      = kind_acked
                             , z_class     = cls
                             , z_instance  = inst
                             , z_recipient = B.pack "*"
                             , z_opcode    = B.pack "auto"
                             , z_fields    = [ B.pack "HsZephyr Stress Test"
                                             , body ]
                             , z_auth      = Unauthenticated
                             }

recvLoop :: Int -> IO ()
recvLoop i = let prefix = (B.pack "[") `B.append` (B.pack $ show i) `B.append` (B.pack "] ") in
             do note   <- receiveNotice
                if (z_opcode note /= B.pack "auto")
                 then do sendZephyrClass (z_class note)
                                         (z_instance note) $
                                         prefix `B.append` (z_fields note !! 1)
                 else return ()
                recvLoop i

printFields :: [B.ByteString] -> IO ()
printFields fields = loop 1 fields
    where loop i (f:fields) = do
            putStr $ "Field " ++ (show i) ++ ": "
            B.putStrLn f
            loop (i+1) fields
          loop i [] = return ()
