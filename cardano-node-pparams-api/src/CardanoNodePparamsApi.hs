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
  EpochNo (..),
  EpochSlots (EpochSlots),
  EraHistory (..),
  ExceptT (..),
  File (File),
  FromJSON,
  Hash (..),
  LocalNodeConnectInfo (LocalNodeConnectInfo),
  MonadIO,
  QueryConvenienceError (..),
  QueryInEra (QueryInShelleyBasedEra),
  QueryInMode (QueryInEra, QuerySystemStart),
  QueryInShelleyBasedEra (QueryEpoch, QueryProtocolParameters),
  ShelleyBasedEra (..),
  SlotNo,
  SlotsInEpoch (..),
  SlotsToEpochEnd (..),
  SystemStart (..),
  ToJSON,
  docToLazyText,
  except,
  executeLocalStateQueryExpr,
  executeQueryAnyMode,
  firstExceptT,
  forEraInEon,
  getLocalChainTip,
  getProgress,
  hoistEither,
  hoistMaybe,
  hushM,
  left,
  lift,
  liftIO,
  makeChainTip,
  onLeft,
  onNothing,
  queryChainBlockNo,
  queryChainPoint,
  queryCurrentEra,
  queryEraHistory,
  queryExpr,
  queryNodeLocalState,
  querySystemStart,
  runExceptT,
  slotToEpoch,
  throwE,
  withExceptT,
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
import Cardano.CLI.Types.Errors.CmdError (CmdError (..))
import Cardano.CLI.Types.Errors.QueryCmdError (
  QueryCmdError (..),
  renderQueryCmdError,
 )
import qualified Cardano.CLI.Types.Output as O (
  QueryTipLocalState (..),
  QueryTipLocalStateOutput (..),
 )
import Cardano.Crypto.Hash (Blake2b_256, digest, hashFromBytes, hashWith)
import Control.Exception (Exception, throwIO)
import Control.Monad (forM, join)
import Control.Monad.Trans.Except.Extra (newExceptT)
import Data.Aeson (encode)
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import Data.Time (getCurrentTime)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Typeable (Typeable, cast, typeOf, typeRep)
import Data.Word (Word32, Word64)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (
  HostPreference,
  defaultSettings,
  runSettings,
  setHost,
  setPort,
  setTimeout,
 )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (
  RelativeTime (..),
  toRelativeTime,
 )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Consensus.HardFork.History (summaryWithExactly)
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
  return $ addHeader cacheHeader tip

-- case tip of
--   Left err -> Handler $ throwE $ err500 {errBody = BLU.fromString $ show err}
--   Right (Left err) -> Handler $ throwE $ err500 {errBody = BLU.fromString $ show err}
--   Right (Right result) -> return $ addHeader cacheHeader result

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

queryTip :: LocalNodeConnectInfo -> IO Tip
queryTip localNodeConn = do
  Right eLocalState <- runExceptT $
    ExceptT $
      fmap sequence $
        executeLocalStateQueryExpr localNodeConn VolatileTip $
          runExceptT $ do
            era <- lift queryCurrentEra & onLeft (left . QueryCmdUnsupportedNtcVersion)
            eraHistory <- lift queryEraHistory & onLeft (left . QueryCmdUnsupportedNtcVersion)
            mChainBlockNo <- lift queryChainBlockNo & onLeft (left . QueryCmdUnsupportedNtcVersion) & fmap Just
            mChainPoint <- lift queryChainPoint & onLeft (left . QueryCmdUnsupportedNtcVersion) & fmap Just
            mSystemStart <- lift querySystemStart & onLeft (left . QueryCmdUnsupportedNtcVersion) & fmap Just

            return
              O.QueryTipLocalState
                { O.era = era
                , O.eraHistory = eraHistory
                , O.mSystemStart = mSystemStart
                , O.mChainTip = makeChainTip <$> mChainBlockNo <*> mChainPoint
                }

  mLocalState <- hushM (first QueryCmdAcquireFailure eLocalState) $ \e ->
    liftIO . LT.hPutStrLn stderr $
      docToLazyText $
        "Warning: Local state unavailable: " <> renderQueryCmdError e

  Right chainTip <-
    runExceptT $
      pure (mLocalState >>= O.mChainTip)
        -- The chain tip is unavailable via local state query because we are connecting with an older
        -- node to client protocol so we use chain sync instead which necessitates another connection.
        -- At some point when we can stop supporting the older node to client protocols, this fallback
        -- can be removed.
        & onNothing (lift $ queryChainTipViaChainSync localNodeConn)

  let (tipSlotNo :: SlotNo, hash :: (Hash BlockHeader), blockNo :: BlockNo) = case chainTip of
        -- How to hash sample data and cast to (Hash BlockHeader?)
        -- ChainTipAtGenesis -> (0, "0", 0)
        ChainTip slotNo hash blockNo -> (slotNo, hash, blockNo)

  Just localStateOutput <- forM mLocalState $ \localState -> do
    case slotToEpoch tipSlotNo (O.eraHistory localState) of
      Left e -> do
        liftIO . LT.hPutStrLn stderr $
          docToLazyText $
            "Warning: Epoch unavailable: " <> renderQueryCmdError (QueryCmdPastHorizon e)
        return $
          O.QueryTipLocalStateOutput
            { O.localStateChainTip = chainTip
            , O.mEra = Nothing
            , O.mEpoch = Nothing
            , O.mSyncProgress = Nothing
            , O.mSlotInEpoch = Nothing
            , O.mSlotsToEpochEnd = Nothing
            }
      Right (epochNo, SlotsInEpoch slotsInEpoch, SlotsToEpochEnd slotsToEpochEnd) -> do
        syncProgressResult <- runExceptT $ do
          systemStart <-
            fmap getSystemStart (O.mSystemStart localState) & hoistMaybe QueryCmdSystemStartUnavailable
          nowSeconds <- toRelativeTime (SystemStart systemStart) <$> liftIO getCurrentTime
          tipTimeResult <-
            getProgress tipSlotNo (O.eraHistory localState) & bimap QueryCmdPastHorizon fst & except

          let tolerance = RelativeTime (secondsToNominalDiffTime 600)

          return $ percentage tolerance tipTimeResult nowSeconds

        mSyncProgress <- hushM syncProgressResult $ \e -> do
          liftIO . LT.hPutStrLn stderr $
            docToLazyText $
              "Warning: Sync progress unavailable: " <> renderQueryCmdError e

        -- Extend past this return structure to include system start utc
        return $
          O.QueryTipLocalStateOutput
            { O.localStateChainTip = chainTip
            , O.mEra = Just (O.era localState)
            , O.mEpoch = Just epochNo
            , O.mSlotInEpoch = Just slotsInEpoch
            , O.mSlotsToEpochEnd = Just slotsToEpochEnd
            , O.mSyncProgress = mSyncProgress
            }

  -- Implement proper exception propagation and handling
  return $
    Tip
      blockNo
      (fromMaybe (EpochNo 0) $ O.mEpoch localStateOutput)
      (fromMaybe (AnyCardanoEra ByronEra) $ O.mEra localStateOutput)
      hash
      tipSlotNo
      (fromMaybe (0 :: Word64) $ O.mSlotInEpoch localStateOutput)
      (fromMaybe (0 :: Word64) $ O.mSlotsToEpochEnd localStateOutput)
      (fromMaybe ("0.00" :: Text) $ O.mSyncProgress localStateOutput)

data Tip = Tip
  { block :: BlockNo
  , epoch :: EpochNo
  , era :: AnyCardanoEra
  , hash :: Hash BlockHeader
  , slot :: SlotNo
  , slotInEpoch :: Word64
  , slotToEpochEnd :: Word64
  , syncProgress :: Text
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

-- | Query the chain tip via the chain sync protocol.
--
-- This is a fallback query to support older versions of node to client protocol.
queryChainTipViaChainSync :: MonadIO m => LocalNodeConnectInfo -> m ChainTip
queryChainTipViaChainSync localNodeConnInfo = do
  liftIO . T.hPutStrLn stderr $
    "Warning: Local header state query unavailable. Falling back to chain sync query"
  liftIO $ getLocalChainTip localNodeConnInfo

instance Exception QueryCmdError

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

-- Alternative query which runs into same constraint issue:
-- Right (AnyCardanoEra era) <- runExceptT $ determineEra localNodeConn
-- sbe <- forEraInEon @ShelleyBasedEra era (throwIO QueryCmdByronEra) pure
-- let qInMode = QueryInEra $ QueryInShelleyBasedEra sbe QueryProtocolParameters
-- pp <- runExceptT $ executeQueryAnyMode localNodeConn qInMode
--
-- For debug outside of the api server:
-- pparams <- queryPparams localNodeConn
-- case pparams of
--   Left err -> print err
--   Right (Left eraMismatch) -> print eraMismatch
--   Right (Right result) -> print $ encode result
