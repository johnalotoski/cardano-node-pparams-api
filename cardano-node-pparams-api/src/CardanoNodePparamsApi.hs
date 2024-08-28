{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

module CardanoNodePparamsApi (main) where

import Cardano.Api (
  AnyCardanoEra (..),
  AnyShelleyBasedEra (..),
  CardanoEra (..),
  ConsensusModeParams (CardanoModeParams),
  EpochSlots (EpochSlots),
  File (File),
  LocalNodeConnectInfo (LocalNodeConnectInfo),
  QueryInEra (QueryInShelleyBasedEra),
  QueryInMode (QueryInEra),
  QueryInShelleyBasedEra (QueryProtocolParameters),
  ShelleyBasedEra (..),
  queryNodeLocalState,
 )

import Cardano.Api.IPC (AcquiringFailure)

import Cardano.Api.Shelley (
  NetworkId (..),
  NetworkMagic (..),
  determineEra,
 )

import Cardano.Api.Network (Target (VolatileTip))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text.Lazy qualified as BL
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import Web.Scotty (get, html, scotty)
import Prelude

-- For debug fns
-- import Data.Typeable (Typeable, typeOf)

defaultCModeParams :: ConsensusModeParams
defaultCModeParams = CardanoModeParams (EpochSlots 21600)

localNodeConnInfo :: FilePath -> Word32 -> LocalNodeConnectInfo
localNodeConnInfo sSocketPath wNetworkId =
  LocalNodeConnectInfo
    defaultCModeParams
    (networkId wNetworkId)
    (File sSocketPath)
  where
    networkId :: Word32 -> NetworkId
    networkId 764824073 = Mainnet
    networkId _ = Testnet $ NetworkMagic wNetworkId

readEnvVarOrExit :: String -> IO String
readEnvVarOrExit sEnvVar = do
  mEnvVar <- lookupEnv sEnvVar
  case mEnvVar of
    Just sResult -> return sResult
    Nothing -> do
      putStrLn $ sEnvVar ++ " is unset.  Please define this environment variable and retry."
      exitFailure

stringToWord32OrExit :: String -> IO Word32
stringToWord32OrExit str = case readMaybe str :: Maybe Word32 of
  Just value -> return value
  Nothing -> do
    putStrLn $ str ++ " is not a valid Word32 networkId.  Please update the network id and retry."
    exitFailure

validateShelleyEraOrExit ::
  Either AcquiringFailure AnyCardanoEra -> IO AnyShelleyBasedEra
validateShelleyEraOrExit era = case era of
  Left err -> print err >> exitFailure
  Right (AnyCardanoEra ByronEra) ->
    putStrLn "Sorry, this server won't work in Bryon era, try something a little newer." >> exitFailure
  Right (AnyCardanoEra ShelleyEra) -> return $ AnyShelleyBasedEra ShelleyBasedEraShelley
  Right (AnyCardanoEra AllegraEra) -> return $ AnyShelleyBasedEra ShelleyBasedEraAllegra
  Right (AnyCardanoEra MaryEra) -> return $ AnyShelleyBasedEra ShelleyBasedEraMary
  Right (AnyCardanoEra AlonzoEra) -> return $ AnyShelleyBasedEra ShelleyBasedEraAlonzo
  Right (AnyCardanoEra BabbageEra) -> return $ AnyShelleyBasedEra ShelleyBasedEraBabbage
  Right (AnyCardanoEra ConwayEra) -> return $ AnyShelleyBasedEra ShelleyBasedEraConway

-- Use Continuation-Passing Style (CPS) approach to avoid Skolem and variable escape errors on unwrap
withAnyShelleyBasedEra :: AnyShelleyBasedEra -> (forall era. ShelleyBasedEra era -> r) -> r
withAnyShelleyBasedEra (AnyShelleyBasedEra era) handler = handler era

processEra :: ShelleyBasedEra era -> String
processEra ShelleyBasedEraShelley = "This is the Shelley era."
processEra ShelleyBasedEraAllegra = "This is the Allegra era."
processEra ShelleyBasedEraMary = "This is the Mary era."
processEra ShelleyBasedEraAlonzo = "This is the Alonzo era."
processEra ShelleyBasedEraBabbage = "This is the Babbage era."
processEra ShelleyBasedEraConway = "This is the Conway era."

-- Debug fns
-- printEither :: Show a => Show b => Either a b -> IO ()
-- printEither (Left a) = putStrLn ("Left: " ++ show a)
-- printEither (Right b) = putStrLn ("Right: " ++ show b)
--
-- printTypeOf :: Typeable a => a -> IO ()
-- printTypeOf x = putStrLn $ "The type is: " ++ show (typeOf x)

main :: IO ()
main = do
  sNetworkId <- readEnvVarOrExit "CARDANO_NODE_NETWORK_ID"
  sSocketPath <- readEnvVarOrExit "CARDANO_NODE_SOCKET_PATH"
  wNetworkId <- stringToWord32OrExit sNetworkId

  let localNode = localNodeConnInfo sSocketPath wNetworkId

  -- TODO: Execute this occasionally to determine if there is an era change
  era <- runExceptT $ determineEra localNode
  queryEra <- validateShelleyEraOrExit era
  print queryEra

  let qX = withAnyShelleyBasedEra queryEra processEra

  print qX
  _ <- exitFailure

  scotty 3000 $
    get "/api/pparams" $ do
      -- Obtain protocol parameters
      pparams <-
        liftIO $
          runExceptT $
            queryNodeLocalState localNode VolatileTip $
              QueryInEra $
                -- QueryInShelleyBasedEra qX QueryProtocolParameters
                QueryInShelleyBasedEra ShelleyBasedEraBabbage QueryProtocolParameters

      -- TODO: use json
      html $ case pparams of
        Left err -> BL.pack $ "Query failed with error: " ++ show err
        Right (Left eraMismatch) -> BL.pack $ "Era mismatch: " ++ show eraMismatch
        Right (Right result) -> BL.pack $ BL8.unpack $ encode result
