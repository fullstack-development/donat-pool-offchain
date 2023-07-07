module Ext.Seriaization.Key where

import Contract.Prelude

import Contract.Monad (liftContractM, Contract)
import Ctl.Internal.Serialization.Hash (ed25519KeyHashToBech32, ed25519KeyHashFromBech32)
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash(..), PubKeyHash(..))

pkhToBech32 ∷ PaymentPubKeyHash → Maybe Bech32String
pkhToBech32 pkh = ed25519KeyHashToBech32 "addr_vkh" (unwrap $ unwrap pkh)

pkhFromBech32 ∷ Bech32String → Maybe PaymentPubKeyHash
pkhFromBech32 pkhStr = (PubKeyHash >>> PaymentPubKeyHash) <$> ed25519KeyHashFromBech32 pkhStr

pkhToBech32M ∷ PaymentPubKeyHash → Contract Bech32String
pkhToBech32M pkh = liftContractM "Impossible to serialize pkh" $ pkhToBech32 pkh

pkhFromBech32M ∷ Bech32String → Contract PaymentPubKeyHash
pkhFromBech32M pkhStr = liftContractM "Impossible to deserialize pkh" $ pkhFromBech32 pkhStr
