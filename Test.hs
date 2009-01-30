import Network.Zephyr as Z
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do Z.initialize
          Z.openPort
          Z.sendNotice note
    where note = Z.emptyNotice { z_kind      = Z.kind_acked
                               , z_class     = B.pack "nelhage-test"
                               , z_instance  = B.pack "hszephyr"
                               , z_recipient = B.pack "*"
                               , z_fields    = map B.pack $ [ "Nelson Elhage"
                                                            , "Hello from Haskell" ]
                               , z_auth      = Authenticated
                               }
