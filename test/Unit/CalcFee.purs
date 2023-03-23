module Test.Unit.CalcFee (suite) where

import Control.Bind (bind, discard)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.ByteArray (hexToByteArrayUnsafe)
import Data.BigInt (fromInt)
import Data.Eq ((==))
import Data.Function (($))
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Newtype (unwrap)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Fundraising.ReceiveFunds as ReceiveFunds
import Mote (test)
-- import Test.Ctl.Utils (assertTrue, errMaybe)

suite :: TestPlanM (Aff Unit) Unit
suite = test "CalcFee tests" do
  assertTrue "ed25519KeyHashFromBech32 returns Nothing on random string"
    (ReceiveFunds.calcFeePure (fromInt 3) (fromInt 1000) == fromInt 30)



assertTrue
  :: forall (m :: Type -> Type)
   . Applicative m
  => MonadEffect m
  => String
  -> Boolean
  -> m Unit
assertTrue msg b = unless b $ liftEffect $ throwException $ error msg









  -- pkh <- errMaybe "ed25519KeyHashFromBech32 failed" $ ed25519KeyHashFromBech32
  --   pkhBech32
  -- let
  --   pkhB32 = ed25519KeyHashToBech32Unsafe "addr_vkh" pkh
  --   mPkhB32 = ed25519KeyHashToBech32 "addr_vkh" pkh
  --   pkhBts = ed25519KeyHashToBytes pkh
  --   pkh2 = ed25519KeyHashFromBytes $ unwrap pkhBts

  -- assertTrue
  --   "Safe ed25519KeyHashToBech32 should produce Just when unsafe version works"
  --   (mPkhB32 == Just pkhB32)

  -- assertTrue
  --   "Safe ed25519KeyHashToBech32 should return Nothing on invalid prefix"
  --   (ed25519KeyHashToBech32 "" pkh == Nothing)