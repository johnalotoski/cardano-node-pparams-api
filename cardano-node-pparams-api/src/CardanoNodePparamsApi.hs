{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module CardanoNodePparamsApi (main) where

import Cardano.Api (
  ConsensusModeParams (CardanoModeParams),
  EpochSlots (EpochSlots),
  File (File),
  LocalNodeConnectInfo (LocalNodeConnectInfo),
  QueryInEra (QueryInShelleyBasedEra),
  QueryInMode (QueryInEra),
  QueryInShelleyBasedEra (QueryEpoch, QueryProtocolParameters),
  ShelleyBasedEra (ShelleyBasedEraConway),
  queryNodeLocalState,
 )

import Cardano.Api.Shelley (
  NetworkId (Mainnet, Testnet),
  NetworkMagic (NetworkMagic),
 )

import Cardano.Api.Network (Target (VolatileTip))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text.Lazy qualified as BL
import Data.Typeable (Typeable, typeOf)
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import Web.Scotty
import Prelude

defaultCModeParams :: ConsensusModeParams
defaultCModeParams = CardanoModeParams (EpochSlots 21600)

stringToWord32OrExit :: String -> IO Word32
stringToWord32OrExit str = case readMaybe str :: Maybe Word32 of
  Just value -> return value
  Nothing -> do
    putStrLn $ str ++ " is not a valid Word32 networkId"
    exitFailure

localNodeConnInfo :: FilePath -> Word32 -> LocalNodeConnectInfo
localNodeConnInfo sSocketPath wNetworkId =
  LocalNodeConnectInfo
    defaultCModeParams
    (Testnet (NetworkMagic wNetworkId))
    (File sSocketPath)

readEnvVarOrExit :: String -> IO String
readEnvVarOrExit sEnvVar = do
  mEnvVar <- lookupEnv sEnvVar
  case mEnvVar of
    Just sResult -> return sResult
    Nothing -> do
      putStrLn $ sEnvVar ++ " is unset"
      exitFailure

-- Debug fns
printEither :: Show a => Show b => Either a b -> IO ()
printEither (Left a) = putStrLn ("Left: " ++ show a)
printEither (Right b) = putStrLn ("Right: " ++ show b)

printTypeOf :: Typeable a => a -> IO ()
printTypeOf x = putStrLn $ "The type is: " ++ show (typeOf x)

main :: IO ()
main = do
  sNetworkId <- liftIO $ readEnvVarOrExit "CARDANO_NODE_NETWORK_ID"
  sSocketPath <- liftIO $ readEnvVarOrExit "CARDANO_NODE_SOCKET_PATH"
  wNetworkId <- liftIO $ stringToWord32OrExit sNetworkId

  scotty 3000 $
    get "/api/pparams" $ do
      -- Obtain protocol parameters
      pparams <-
        liftIO $
          runExceptT $
            queryNodeLocalState (localNodeConnInfo sSocketPath wNetworkId) VolatileTip $
              QueryInEra $
                QueryInShelleyBasedEra ShelleyBasedEraConway QueryProtocolParameters

      -- TODO: use json
      html $ case pparams of
        Left err -> BL.pack $ "Query failed with error: " ++ show err
        Right (Left eraMismatch) -> BL.pack $ "Era mismatch: " ++ show eraMismatch
        Right (Right pparams) -> BL.pack $ BL8.unpack $ encode pparams
