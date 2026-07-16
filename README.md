# Bank Mellat API in Haskell

It is a Haskell client library for consuming Bank Mellat payment SOAP API
to provide payment capability of Bank Mellat to your application.

## Development

Enter to dev environment:

```bash
nix develop
```

## Example of usage

The follow is like that:

First, set up a hook to paymentCallback (defined below) and set the callbackUrl field in BankMellatConfig to point to it. Then, call deposit from Haskell. After that, call the paymentPage function in Haskell, or an equivalent function such as startPayment in TypeScript if you're using TypeScript for the frontend. This will redirect the user to the payment page. Once the payment process is complete, the paymentCallback hook will be invoked with a application/x-www-form-urlencoded payload. For more information, refer to the Bank Mellat API documentation.

```haskell
import Network.SOAP.Transport.HTTP qualified as SOAP
import Control.Monad.Logger (runStdoutLoggingT)
import Bank.Mellat

SaleRefID   = Text
RedirectUrl = Text
MobileNo    = Text

deposit :: Int -> Int -> BankMellatConfig -> IO (SaleRefID, RedirectUrl, MobileNo)
deposit payerId amount bankMellatConfig = do
    now <- getCurrentTime
    let mobileNo = "989913455321"
    let payReq =
            PayRequest
                { orderId = 2
                , amount
                , createdAt = now
                , additionalData = "deposit for wallet 241234"
                , payerId
                , mobileNo
                }
    transport <- SOAP.initTransport_ (T.unpack bankMellatConfig.serviceUrl)
    mPayResp <- runStdoutLoggingT $ bpPayRequest bankMellatConfig transport payReq

    PayResponse {resCode, saleReferenceId} <- case mPayResp of
        Left _e -> error "Internal Server Error"
        Right x -> pure x

    if resCode == 0
        then pure (saleReferenceId, bankMellatConfig.redirectUrl, mobileNo)
        else error ("PayRequestFailed:" <> read resCode)

paymentCallback :: Int -> Int -> Text -> BankMellatConfig -> IO SaleRefID
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

```typescript
export const startPayment = (redirectUrl: string, refId: string, mobileNo: string) => {
  const form = document.createElement('form');
  form.method = 'POST';
  form.action = redirectUrl;

  const fields = {
    RefId: refId,
    MobileNo: mobileNo,
  };

  for (const name in fields) {
    const input = document.createElement('input');
    input.type = 'hidden';
    input.name = name;
    // @ts-ignore
    input.value = fields[name];
    form.appendChild(input);
  }

  document.body.appendChild(form);
  form.submit();
};
```
