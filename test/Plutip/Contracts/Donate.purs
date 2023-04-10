module Test.Plutip.Contracts.Donate where

import Prelude

import Contract.Test.Plutip (InitialUTxOs, withWallets, withStakeKey)
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (try)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Tuple.Nested (type (/\), (/\))
import Data.BigInt as BigInt
import Fundraising.Create as Create
import Fundraising.Donate as Donate
import Fundraising.UserData (CreateFundraisingParams(..), FundraisingData(..))
import Mote (group, test)
import Protocol.StartProtocol as StartProtocol
import Shared.Duration (Duration(..))
import Test.Plutip.Contracts.StartProtocol (startProtocolParams)
import Test.Plutip.Contracts.UpdateProtocol (incorrectProtocol)
import Test.Plutip.Utils (isExpectedError)
import Test.Spec.Assertions (shouldSatisfy, shouldEqual)
import Ctl.Internal.Test.ContractTest (ContractTest)
import Test.Plutip.Contracts.CreateFundraising
import Data.BigInt (fromInt)
import Test.Plutip.Common (privateStakeKey)

suite :: TestPlanM ContractTest Unit
suite = do
  group "Create Fundraising" do

    test "Should successfully create a new project" do
      let
        aliceUtxos =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
        bobUtxos =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
        distribution =
          withStakeKey privateStakeKey aliceUtxos
            /\ withStakeKey privateStakeKey bobUtxos
      withWallets distribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.contract startProtocolParams
        frData <- withKeyWallet bob $ Create.contract protocol (mkFundraisingParams 100 (mkFundraisingDuration 0 1 0))
        withKeyWallet alice $ void $ Donate.contract frData (fromInt 20)
 
