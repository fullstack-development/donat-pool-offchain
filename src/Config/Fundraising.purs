module Config.Fundraising where

import Contract.Prelude

import Contract.Address (Bech32String, addressToBech32, getNetworkId, validatorHashBaseAddress)
import Contract.Monad (Contract, liftContractM)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Effect (Effect)
import Ext.Contract.Value (currencySymbolToString, tokenNameToString, mkCurrencySymbol)
import Fundraising.FundraisingScript (getFundraisingTokenName, getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import MintingPolicy.VerTokenMinting as VerToken
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Protocol.Models (Protocol)

type FundraisingConfig =
  { address :: Bech32String
  , verTokenCurrency :: Bech32String
  , verTokenName :: String
  , threadTokenName :: String
  }

writeFundraisingConfig :: FundraisingConfig -> Effect Unit
writeFundraisingConfig fundraisingConfig = do
  let jfundraisingConfig = stringify $ encodeJson fundraisingConfig
  liftEffect $ writeTextFile UTF8 "conf/fundraising.local.conf" jfundraisingConfig

makeFundraisingConfig ∷ Protocol → Contract FundraisingConfig
makeFundraisingConfig protocol = do
  _ /\ verTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  let verTokenBech32 = currencySymbolToString verTokenCs
  verTn <- VerToken.verTokenName
  verTnString <- liftContractM "Impossible to decode fundraising ver token name" $ tokenNameToString verTn

  let
    fundraising = Fundraising
      { protocol: protocol
      , verTokenCurrency: verTokenCs
      , verTokenName: verTn
      }
  networkId <- getNetworkId
  frValidatorHash <- getFundraisingValidatorHash fundraising
  frAddress <- liftContractM "Impossible to get Fundraising script address" $ validatorHashBaseAddress networkId frValidatorHash
  bech32Address <- addressToBech32 frAddress

  nftTn <- getFundraisingTokenName
  nftTnString <- liftContractM "Impossible to decode fundraising thread token name" $ tokenNameToString nftTn

  pure $
    { address: bech32Address
    , verTokenCurrency: verTokenBech32
    , verTokenName: verTnString
    , threadTokenName: nftTnString
    }
