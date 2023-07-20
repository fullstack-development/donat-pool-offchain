module Shared.NetworkData where

import Contract.Address (NetworkId(..))
import Contract.Prelude

newtype NetworkParams = NetworkParams
  { wallet :: String
  , isMainnet :: Boolean
  }

derive newtype instance Show NetworkParams
derive instance Generic NetworkParams _

data WalletType = Nami | Flint | Lode | Eternl

derive instance Eq WalletType
derive instance Generic WalletType _

instance Show WalletType where
  show = genericShow

newtype NetworkWallet = NetworkWallet
  { networkId :: NetworkId
  , walletType :: WalletType
  }

derive newtype instance Show NetworkWallet
derive newtype instance Eq NetworkWallet

networkParamsToNetworkWallet :: NetworkParams -> Maybe NetworkWallet
networkParamsToNetworkWallet (NetworkParams { wallet, isMainnet }) =
  case (wallet /\ isMainnet) of
    ("Nami" /\ false) -> Just $ NetworkWallet { networkId: TestnetId, walletType: Nami }
    ("Flint" /\ false) -> Just $ NetworkWallet { networkId: TestnetId, walletType: Flint }
    ("Lode" /\ false) -> Just $ NetworkWallet { networkId: TestnetId, walletType: Lode }
    ("Eternl" /\ false) -> Just $ NetworkWallet { networkId: TestnetId, walletType: Eternl }
    ("Nami" /\ true) -> Just $ NetworkWallet { networkId: MainnetId, walletType: Nami }
    ("Flint" /\ true) -> Just $ NetworkWallet { networkId: MainnetId, walletType: Flint }
    ("Lode" /\ true) -> Just $ NetworkWallet { networkId: MainnetId, walletType: Lode }
    ("Eternl" /\ true) -> Just $ NetworkWallet { networkId: MainnetId, walletType: Eternl }
    _ -> Nothing
