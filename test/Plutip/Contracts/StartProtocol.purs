module Test.Plutip.Contracts.StartProtocol (suite, startProtocolParams) where

import Prelude

import Contract.Monad (Contract)
import Contract.Test.Plutip (InitialUTxOs, withWallets)
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (try)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.BigInt as BigInt
import Mote (group, test)
import Protocol.StartProtocol as StartProtocol
import Protocol.UserData (ProtocolConfigParams(..))
import Test.Plutip.Utils (isExpectedError)
import Test.Spec.Assertions (shouldSatisfy)
import Ctl.Internal.Test.ContractTest (ContractTest)

suite :: TestPlanM ContractTest Unit
suite = do
  group "Start Protocol" do

    test "Should successfully start protocol" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      withWallets distribution \alice -> withKeyWallet alice $ startProtocolContract startProtocolParams

    test "Should fail if user wallet doesn't have any UTxOs" do
      let
        distribution :: InitialUTxOs
        distribution = []
      withWallets distribution \alice -> do
        result <- try $ withKeyWallet alice $ startProtocolContract startProtocolParams
        let errMsg = "Utxo set is empty"
        result `shouldSatisfy` (isExpectedError errMsg)

startProtocolParams :: ProtocolConfigParams
startProtocolParams =
  ProtocolConfigParams
    { minAmountParam: BigInt.fromInt 2_000_000
    , maxAmountParam: BigInt.fromInt 100_000_000
    , minDurationParam: BigInt.fromInt 5 -- minutes
    , maxDurationParam: BigInt.fromInt 250 -- minutes
    , protocolFeeParam: BigInt.fromInt 5 -- percentage
    }

startProtocolContract
  :: ProtocolConfigParams
  -> Contract Unit
startProtocolContract = void <<< StartProtocol.contract
