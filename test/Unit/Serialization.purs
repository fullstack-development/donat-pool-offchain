module Test.Unit.Serialization (suite) where

import Prelude

import Ctl.Internal.Serialization.Hash (Ed25519KeyHash, ed25519KeyHashFromBytes)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.ByteArray (hexToByteArrayUnsafe)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash(..), PubKeyHash(..))
import Effect.Aff (Aff)
import Mote (test)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)
import Data.Maybe (fromJust, maybe)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Ext.Seriaization.Key (pkhFromBech32, pkhToBech32)

pk :: Ed25519KeyHash
pk = unsafePartial $ fromJust $ ed25519KeyHashFromBytes $
  hexToByteArrayUnsafe
    "49d49d1715768d0b9fb498e60a7515e390c744330b91f4a1f6329afa"

pkh ∷ PaymentPubKeyHash
pkh = PaymentPubKeyHash $ PubKeyHash pk

suite :: TestPlanM (Aff Unit) Unit
suite =
  test "Pkh serialization tests" do
    bech32Pkh <- maybe (liftEffect $ throw "Can't serialize pkh to Bech32") pure $ pkhToBech32 pkh
    resultPkh <- maybe (liftEffect $ throw "Can't deserialize pkh from Bech32") pure $ pkhFromBech32 bech32Pkh
    resultPkh `shouldEqual` pkh
