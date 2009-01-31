import Network.Zephyr
import Control.Monad
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
          subscribeTo [ZSubscription (B.pack "nelhage-test") (B.pack "*") (B.pack "*")]
          recvLoop
          -- sendNotice note
    where note = emptyNotice { z_kind      = kind_acked
                             , z_class     = B.pack "nelhage-test"
                             , z_instance  = B.pack "hszephyr"
                             , z_recipient = B.pack "*"
                             , z_fields    = map B.pack $ [ "Nelson Elhage"
                                                          , "Hello from Haskell" ]
                             , z_auth      = Authenticated
                             }

recvLoop :: IO ()
recvLoop = do note   <- receiveNotice
              putStrLn $ "Zephyr from " ++ (B.unpack $ fromJust $ z_sender note) ++ ":"
              putStrLn $ "Auth: " ++ (show $ z_auth note)
              printFields $ z_fields note
              hFlush stdout
              recvLoop

printFields :: [B.ByteString] -> IO ()
printFields fields = loop 1 fields
    where loop i (f:fields) = do
            putStr $ "Field " ++ (show i) ++ ": "
            B.putStrLn f
            loop (i+1) fields
          loop i [] = return ()
