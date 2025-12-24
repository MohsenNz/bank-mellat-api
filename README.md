# Bank Mellat API in Haskell

It is a Haskell client library for consuming Bank Mellat payment SOAP API
to provide payment capability of Bank Mellat to your application.

## Development

Enter to dev environment:

```bash
nix develop
```

## Example of usage

```Haskell
import Network.SOAP.Transport.HTTP qualified as SOAP
import Control.Monad.Logger (runStdoutLoggingT)
import Bank.Mellat

deposit :: Int -> Int -> BankMellatConfig -> IO (Text, Text)
deposit payerId amount bankMellatConfig = do
    now <- getCurrentTime
    let payReq =
            PayRequest
                { orderId = 2
                , amount
                , createdAt = now
                , additionalData = "deposit for wallet 241234"
                , payerId
                , mobileNo = "989913455321"
                }
    transport <- SOAP.initTransport_ (T.unpack bankMellatConfig.serviceUrl)
    mPayResp <- runStdoutLoggingT $ bpPayRequest bankMellatConfig transport payReq

    PayResponse {resCode, saleReferenceId} <- case mPayResp of
        Left _e -> error "Internal Server Error"
        Right x -> pure x

    if resCode == 0
        then pure (saleReferenceId, bankMellatConfig.redirectUrl)
        else error ("PayRequestFailed:" <> read resCode)

paymentCallback :: Int -> Int -> Text -> BankMellatConfig -> IO Text
paymentCallback resCode saleOrderId saleReferenceId bankMellatConfig = do
    t <- SOAP.initTransport_ (T.unpack bankMellatConfig.serviceUrl)
    let
        req = BankRequest {saleOrderId, saleReferenceId}
        conf = bankMellatConfig
    unless (resCode == 0) $ error "PaymentFailed"
    verifyCode <- bpVerifyRequest conf t req
    unless (verifyCode == 0) $ do
        inquiryCode <- bpInquiryRequest conf t req
        unless (inquiryCode == 0) $ error "PaymentVerifaictionFailed"
    settleCode <- bpSettleRequest conf t req
    unless (settleCode == 0) $ do
        _code <- bpReversalRequest conf t req
        error "PaymentReversed"
    pure saleReferenceId
```
