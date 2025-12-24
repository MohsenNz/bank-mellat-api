module Bank.Mellat (
    bpPayRequest,
    bpVerifyRequest,
    bpSettleRequest,
    bpInquiryRequest,
    bpReversalRequest,
    paymentPage,
    PayRequest (..),
    PayResponse (..),
    BankRespCode,
    BankRequest (..),
    BankMellatConfig (..),
) where

import           Control.Exception           (Exception)
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Control.Monad.Logger        (MonadLogger, logDebugN)
import           Data.Bifunctor              (first)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.Data                   (Typeable)
import           Data.Function               ((&))
import           Data.Text                   (Text, splitOn)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import qualified Data.Text.Lazy              as TL
import           Data.Time                   (UTCTime, defaultTimeLocale,
                                              formatTime, getCurrentTimeZone,
                                              utcToLocalTime)
import           GHC.Generics                (Generic)
import           Network.HTTP.Client         (Request (method, requestBody, requestHeaders),
                                              RequestBody (RequestBodyBS),
                                              httpLbs, newManager, parseRequest)
import qualified Network.HTTP.Client         as Client
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Types          (hContentType, methodPost)
import           Network.SOAP                (ResponseParser (StreamParser),
                                              Transport, invokeWS)
import           Network.SOAP.Parsing.Stream (Event, Sink)
import           Text.Blaze.Html             (Html,
                                              ToMarkup (preEscapedToMarkup))
import           Text.Read                   (readEither)
import qualified Text.XML                    as XML
import qualified Text.XML.Stream.Parse       as Parse
import           Text.XML.Stream.Parse       (tagIgnoreAttrs, tagNoAttr)
import           Text.XML.Writer             (element, soap)

-------------------------------------------------------------------------------
-- Bank SOAP API

namespace :: Text
namespace = "http://interfaces.core.sw.bps.com/"

xmlNameNS :: Text -> XML.Name
xmlNameNS n = XML.Name n (Just namespace) Nothing

xmlName :: Text -> XML.Name
xmlName x = XML.Name x Nothing Nothing

bpPayRequest
    :: (MonadIO m, MonadLogger m)
    => BankMellatConfig -> Transport -> PayRequest -> m (Either ParseError PayResponse)
bpPayRequest conf t req = do
    tz <- liftIO getCurrentTimeZone
    let
        localTime' = utcToLocalTime tz req.createdAt
        localTime = T.pack $ formatTime defaultTimeLocale "%H%M%S" localTime'
        localDate = T.pack $ formatTime defaultTimeLocale "%Y%m%d" localTime'
        body = element (xmlNameNS "bpPayRequest") $ do
            element "terminalId"     conf.terminalId
            element "userName"       conf.userName
            element "userPassword"   conf.userPassword
            element "callBackUrl"    conf.callbackUrl
            element "orderId"        req.orderId
            element "amount"         req.amount
            element "localDate"      localDate
            element "localTime"      localTime
            element "additionalData" req.additionalData
            element "payerId"        req.payerId
            element "mobileNo"       req.mobileNo
        soapDoc = soap () body -- For logging
        parser = StreamParser $ parseResponse "bpPayRequestResponse"

    logDebugN $ documentToText soapDoc
    resp <- liftIO $ invokeWS t "" () body parser
    pure $ payRespFromStr resp

bpVerifyRequest, bpSettleRequest, bpInquiryRequest, bpReversalRequest
        :: (MonadIO m)
        => BankMellatConfig -> Transport -> BankRequest -> m BankRespCode
bpVerifyRequest   = request1 "bpVerifyRequest"
bpSettleRequest   = request1 "bpSettleRequest"
bpInquiryRequest  = request1 "bpInquiryRequest"
bpReversalRequest = request1 "bpReversalRequest"

request1
    :: (MonadIO m)
    => Text -> BankMellatConfig -> Transport -> BankRequest -> m Int
request1 name conf t req = liftIO $ invokeWS t "" () body parser
  where
    body = element (xmlNameNS name) $ do
        element "terminalId"      conf.terminalId
        element "userName"        conf.userName
        element "userPassword"    conf.userPassword
        element "orderId"         req.saleOrderId
        element "saleOrderId"     req.saleOrderId
        element "saleReferenceId" req.saleReferenceId

    parser = StreamParser parseResponseIgnoreName

-- TODO: Sink deprecated
parseResponseIgnoreName :: (MonadThrow m) => Sink Event m Int
parseResponseIgnoreName =
    tagNoAttr "return" (read . T.unpack <$> Parse.content) -- NOTE: read is a partial function
        & Parse.force "Missing <return>"
        & tagIgnoreAttrs Parse.anyName
        & Parse.force "Missing response"

-- TODO: Sink deprecated
parseResponse :: (MonadThrow m) => Text -> Sink Event m Text
parseResponse name =
    tagNoAttr "return" Parse.content
        & Parse.force "Missing return"
        & tagNoAttr name'
        & Parse.force ("Missing " <> T.unpack name)
  where
    name' :: Parse.NameMatcher XML.Name = pure $ xmlName name

paymentPage :: Text -> Text -> Text -> Text -> IO Html
paymentPage bankRedirectUrl refId mobileNo merchantName = do
    manager <- newManager tlsManagerSettings
    initReq <- parseRequest (T.unpack bankRedirectUrl)
    let
        formBody =
            T.unpack $
                "RefId="
                    <> refId
                    <> "&MobileNo="
                    <> mobileNo
                    <> "&merchantName"
                    <> merchantName
        req =
            initReq
                { method = methodPost
                , requestBody = RequestBodyBS (BS.pack formBody)
                , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
                }
    resp <- httpLbs req manager
    pure $ htmlFromRaw resp

-------------------------------------------------------------------------------
-- Types

data BankMellatConfig = BankMellatConfig
    { terminalId   :: Int
    , userName     :: Text
    , userPassword :: Text
    , serviceUrl   :: Text
    , redirectUrl  :: Text
    , callbackUrl  :: Text
    } deriving (Generic, Show)

-- TODO(w8p7q8): change @orderId from Int to Int64,
-- currently we use Int because ToXML is not implement for Int64
data PayRequest = PayRequest
    { orderId        :: Int     -- should be unique in each request
    , amount         :: Int
    , createdAt      :: UTCTime
    , additionalData :: Text
    , payerId        :: Int
    , mobileNo       :: Text
    }

data PayResponse = PayResponse
    { resCode         :: Int
    , saleReferenceId :: Text
    } deriving (Show)

-- TODO(w8p7q8): change @saleOrderId from Int to Int64,
data BankRequest = GenericRequest
    { saleOrderId     :: Int
    , saleReferenceId :: Text
    }

type BankRespCode = Int

-------------------------------------------------------------------------------
-- Error

newtype ParseError = ParseError Text
    deriving (Show, Typeable)
    deriving anyclass (Exception)

-------------------------------------------------------------------------------
-- Utile

payRespFromStr :: Text -> Either ParseError PayResponse
payRespFromStr x = case splitOn "," x of
    [resCode]        -> mk resCode ""
    [resCode, refId] -> mk resCode refId
    _                -> Left $ ParseError $ "Can not parse PayResponse from: " <> x
  where
    mk resCode' saleReferenceId =
        case readEitherT resCode' :: Either Text Int of
            Right resCode -> Right $ PayResponse {resCode, saleReferenceId}
            Left e        -> Left $ ParseError e

documentToText :: XML.Document -> Text
documentToText doc =
    TL.toStrict $ XML.renderText Parse.def doc

readEitherT :: (Read a) => Text -> Either Text a
readEitherT x = first T.pack $ readEither (T.unpack x)

htmlFromRaw :: Client.Response LBS.ByteString -> Html
htmlFromRaw resp = do
    let
        body :: BS.ByteString
        body = BS.toStrict $ Client.responseBody resp

        -- -- convert lazy ByteString to Text assuming UTF-8 encoding
        htmlText :: Text
        htmlText = decodeUtf8 body

    preEscapedToMarkup htmlText
