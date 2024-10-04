{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module CardanoNodePparamsApi (main) where

import Cardano.Api (
  AnyCardanoEra (..),
  AnyShelleyBasedEra (..),
  BlockHeader,
  BlockNo,
  CardanoEra (..),
  ChainTip (..),
  ConsensusModeParams (CardanoModeParams),
  ConwayEra,
  EpochNo,
  EpochSlots (EpochSlots),
  ExceptT (..),
  File (File),
  FromJSON,
  Hash,
  LocalNodeConnectInfo (LocalNodeConnectInfo),
  QueryInEra (QueryInShelleyBasedEra),
  QueryInMode (QueryInEra),
  QueryInShelleyBasedEra (QueryEpoch, QueryProtocolParameters),
  ShelleyBasedEra (..),
  SlotNo,
  ToJSON,
  docToLazyText,
  executeLocalStateQueryExpr,
  getLocalChainTip,
  left,
  lift,
  liftIO,
  makeChainTip,
  onLeft,
  queryChainBlockNo,
  queryChainPoint,
  queryCurrentEra,
  queryEraHistory,
  queryNodeLocalState,
  querySystemStart,
  runExceptT,
  throwE,
 )
import Cardano.Api.IPC (AcquiringFailure (..))
import Cardano.Api.Ledger (
  PParams,
  StandardCrypto,
 )
import Cardano.Api.Network (Target (VolatileTip))
import Cardano.Api.Shelley (
  NetworkId (..),
  NetworkMagic (..),
  determineEra,
 )
import Cardano.CLI.EraBased.Run.Query (percentage)
import Cardano.CLI.Types.Errors.QueryCmdError (QueryCmdError (..), renderQueryCmdError)
import qualified Cardano.CLI.Types.Output as O (QueryTipLocalState (..))
import Data.Aeson (encode)
import Data.Bifunctor (first)
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Function ((&))
import Data.Proxy
import Data.String (fromString)
import qualified Data.Text.Lazy.IO as LT
import Data.Typeable (Typeable, cast, typeOf, typeRep)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (
  HostPreference,
  defaultSettings,
  runSettings,
  setHost,
  setPort,
  setTimeout,
 )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Consensus.Shelley.Eras (StandardConway)
import Servant (
  Get,
  Handler (..),
  Header,
  Headers,
  JSON,
  Server,
  addHeader,
  err500,
  errBody,
  serve,
  (:<|>) (..),
  (:>),
 )
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Read (readMaybe)
import Prelude

type Api =
  "era" :> Get '[JSON] (Headers '[Header "Cache-Control" String] AnyCardanoEra)
    :<|> "protocol-parameters"
      :> Get '[JSON] (Headers '[Header "Cache-Control" String] (PParams StandardConway))
    :<|> "tip"
      :> Get '[JSON] (Headers '[Header "Cache-Control" String] Tip)

apiServer :: LocalNodeConnectInfo -> Server Api
apiServer localNodeConn =
  fetchEraHandler localNodeConn
    :<|> fetchPparamsHandler localNodeConn
    :<|> fetchTipHandler localNodeConn

api :: Proxy Api
api = Proxy :: Proxy Api

cacheHeader :: String
cacheHeader = "public, max-age=20"

fetchEraHandler ::
  LocalNodeConnectInfo -> Handler (Headers '[Header "Cache-Control" String] AnyCardanoEra)
fetchEraHandler localNodeConn = do
  era <- liftIO $ queryEra localNodeConn

  case era of
    Left err -> Handler $ throwE $ err500 {errBody = BLU.fromString $ show err}
    Right result -> return $ addHeader cacheHeader result

fetchPparamsHandler ::
  LocalNodeConnectInfo -> Handler (Headers '[Header "Cache-Control" String] (PParams StandardConway))
fetchPparamsHandler localNodeConn = do
  pparams <- liftIO $ queryPparams localNodeConn

  case pparams of
    Left err -> Handler $ throwE $ err500 {errBody = BLU.fromString $ show err}
    Right (Left eraMismatch) -> Handler $ throwE $ err500 {errBody = BLU.fromString $ show eraMismatch}
    Right (Right result) -> return $ addHeader cacheHeader result

fetchTipHandler :: LocalNodeConnectInfo -> Handler (Headers '[Header "Cache-Control" String] Tip)
fetchTipHandler localNodeConn = do
  tip <- liftIO $ queryTip localNodeConn

  case tip of
    Left err -> Handler $ throwE $ err500 {errBody = BLU.fromString $ show err}
    Right (Left err) -> Handler $ throwE $ err500 {errBody = BLU.fromString $ show err}
    Right (Right result) -> return $ addHeader cacheHeader result

-- TODO: make the sbe dynamic
queryEpoch ::
  LocalNodeConnectInfo -> IO (Either AcquiringFailure (Either EraMismatch EpochNo))
queryEpoch localNodeConn = do
  liftIO $
    runExceptT $
      queryNodeLocalState localNodeConn VolatileTip $
        QueryInEra $
          QueryInShelleyBasedEra ShelleyBasedEraConway QueryEpoch

queryEra :: LocalNodeConnectInfo -> IO (Either AcquiringFailure AnyCardanoEra)
queryEra localNodeConn = do
  runExceptT $ determineEra localNodeConn

queryPparams ::
  LocalNodeConnectInfo -> IO (Either AcquiringFailure (Either EraMismatch (PParams StandardConway)))
queryPparams localNodeConn = do
  liftIO $
    runExceptT $
      queryNodeLocalState localNodeConn VolatileTip $
        QueryInEra $
          QueryInShelleyBasedEra ShelleyBasedEraConway QueryProtocolParameters

-- TODO: improve the separate queries into one and add slotInEpoch, slotsToEpochEnd, syncProgress
queryTip :: LocalNodeConnectInfo -> IO (Either AcquiringFailure (Either EraMismatch Tip))
queryTip localNodeConn = do
  -- eLocalState <- ExceptT $
  --   -- eLocalState <-
  --   fmap sequence $
  --     executeLocalStateQueryExpr localNodeConn VolatileTip $
  --       runExceptT $ do
  --         era <- lift queryCurrentEra & onLeft (left . QueryCmdUnsupportedNtcVersion)
  --         eraHistory <- lift queryEraHistory & onLeft (left . QueryCmdUnsupportedNtcVersion)
  --         mChainBlockNo <- lift queryChainBlockNo & onLeft (left . QueryCmdUnsupportedNtcVersion) & fmap Just
  --         mChainPoint <- lift queryChainPoint & onLeft (left . QueryCmdUnsupportedNtcVersion) & fmap Just
  --         mSystemStart <- lift querySystemStart & onLeft (left . QueryCmdUnsupportedNtcVersion) & fmap Just

  --         return
  --           O.QueryTipLocalState
  --             { O.era = era
  --             , O.eraHistory = eraHistory
  --             , O.mSystemStart = mSystemStart
  --             , O.mChainTip = makeChainTip <$> mChainBlockNo <*> mChainPoint
  --             }

  -- mLocalState <- hushM (first QueryCmdAcquireFailure eLocalState) $ \e ->
  --   liftIO . LT.hPutStrLn stderr $
  --     docToLazyText $
  --       "Warning: Local state unavailable: " <> renderQueryCmdError e

  -- chainTip <-
  --   pure (mLocalState >>= O.mChainTip)
  --     & onNothing (queryChainTipViaChainSync localNodeConnInfo)

  era' <- runExceptT $ determineEra localNodeConn
  epoch' <- queryEpoch localNodeConn
  chainTip' <- runExceptT $ getLocalChainTip localNodeConn

  case (era', epoch', chainTip') of
    (Left err, _, _) -> return $ Left err
    (_, Left err, _) -> return $ Left err
    (_, _, Left err) -> return $ Left err
    (_, Right (Left eraMismatch), _) -> return $ Right $ Left eraMismatch
    (Right era, Right (Right epoch), Right chainTip) -> case chainTip of
      ChainTipAtGenesis -> return $ Left AFPointTooOld
      ChainTip slot hash block -> return $ Right $ Right $ Tip block epoch era hash slot

data Tip = Tip
  { block :: BlockNo
  , epoch :: EpochNo
  , era :: AnyCardanoEra
  , hash :: Hash BlockHeader
  , slot :: SlotNo
  }
  deriving (Show, Generic)

instance ToJSON Tip

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

readEnvVarOrExit :: forall a. (Read a, Typeable a) => String -> IO a
readEnvVarOrExit sEnvVar = do
  mEnvVar <- lookupEnv sEnvVar
  case mEnvVar of
    Nothing -> do
      putStrLn $ sEnvVar ++ " is unset.  Please define this environment variable and retry."
      exitFailure
    Just s -> case cast s of
      -- Avoid forcing the user to include double quotes if type is string
      Just s -> return s
      Nothing -> case readMaybe s of
        Nothing -> error $ "Failed to parse: \"" ++ s ++ "\" as type " ++ show (typeRep (Proxy :: Proxy a))
        Just v -> return v

validateShelleyEraOrExit ::
  Either AcquiringFailure AnyCardanoEra -> IO ()
validateShelleyEraOrExit era = case era of
  Left err -> print err >> exitFailure
  Right (AnyCardanoEra ByronEra) ->
    putStrLn "Sorry, this server won't work in Bryon era, try something a little newer." >> exitFailure
  Right (AnyCardanoEra era) -> putStrLn ("Currently in CardanoEra: " ++ show era)

-- Debug fns
-- printEither :: Show a => Show b => Either a b -> IO ()
-- printEither (Left a) = putStrLn ("Left: " ++ show a)
-- printEither (Right b) = putStrLn ("Right: " ++ show b)

main :: IO ()
main = do
  bindHost <- readEnvVarOrExit @String "WARP_BIND_HOST"
  bindPort <- readEnvVarOrExit @Int "WARP_BIND_PORT"
  networkId <- readEnvVarOrExit @Word32 "CARDANO_NODE_NETWORK_ID"
  socketPath <- readEnvVarOrExit @String "CARDANO_NODE_SOCKET_PATH"
  timeout <- readEnvVarOrExit @Int "WARP_TIMEOUT"

  let localNodeConn = localNodeConnInfo socketPath networkId
      settings =
        setTimeout timeout $
          setHost (Data.String.fromString bindHost) $
            setPort bindPort defaultSettings

  era <- runExceptT $ determineEra localNodeConn
  validateShelleyEraOrExit era

  runSettings settings $ serve api $ apiServer localNodeConn

-- For debug outside of the api server:
-- pparams <- queryPparams localNodeConn
-- case pparams of
--   Left err -> print err
--   Right (Left eraMismatch) -> print eraMismatch
--   Right (Right result) -> print $ encode result
