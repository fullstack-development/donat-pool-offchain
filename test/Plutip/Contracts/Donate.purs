module Test.Plutip.Contracts.Donate where

import Data.BigInt (fromInt)
import Prelude
import Test.Plutip.Contracts.CreateFundraising (mkFundraisingDuration, mkFundraisingParams)
import Contract.Monad (Contract)
import Contract.Test.Plutip (withWallets)
import Contract.Value as Value
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (try)
import Ctl.Internal.Test.ContractTest (ContractTest)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.BigInt as BigInt
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(Milliseconds), delay)
import Effect.Aff.Class (liftAff)
import Fundraising.Create as Create
import Fundraising.Donate as Donate
import Fundraising.UserData (FundraisingData(..))
import Mote (group, test)
import Protocol.Models (Protocol)
import Protocol.StartProtocol as StartProtocol
import Shared.Helpers as Helpers
import Test.Plutip.Contracts.StartProtocol (startProtocolParams)
import Test.Plutip.Utils (isExpectedError)
import Test.Spec.Assertions (shouldSatisfy)
import Protocol.UserData (ProtocolConfigParams(..))
import Test.Plutip.Fixtures (distribution)

suite :: TestPlanM ContractTest Unit
suite = do
  group "Create Fundraising" do

    test "Should successfully create a new project" do
      withWallets distribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.contract startProtocolParams
        frData <- withKeyWallet bob $ Create.contract protocol (mkFundraisingParams 100 (mkFundraisingDuration 0 0 10))
        withKeyWallet alice $ void $ Donate.contract frData (fromInt 20)

    test "Should successfully donate more than fundraising goal" do
      withWallets distribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.contract startProtocolParams
        frData <- withKeyWallet bob $ Create.contract protocol (mkFundraisingParams 80 (mkFundraisingDuration 0 0 10))
        withKeyWallet alice $ void $ Donate.contract frData (fromInt 100)

    test "Should successfully donate by fundrising creator" do
      withWallets distribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.contract startProtocolParams
        frData <- withKeyWallet bob $ Create.contract protocol (mkFundraisingParams 80 (mkFundraisingDuration 0 0 10))
        withKeyWallet bob $ void $ Donate.contract frData (fromInt 50)

    test "Should fail if fundrising does not exist" do
      withWallets distribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.contract startProtocolParams
        frData <- incorrectFundrisingData protocol
        result <- try $ withKeyWallet bob $ void $ Donate.contract frData (fromInt 50)
        let errMsg = "Fundraising UTxO with given nft not found"
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should fail if fundraising goal is already reached" do
      withWallets distribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.contract startProtocolParams
        frData <- withKeyWallet bob $ Create.contract protocol (mkFundraisingParams 80 (mkFundraisingDuration 0 0 10))
        withKeyWallet alice $ void $ Donate.contract frData (fromInt 80)
        result <- try $ withKeyWallet alice $ void $ Donate.contract frData (fromInt 20)
        let errMsg = "fundraising goal is already completed"
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should fail if fundraising time is over" do
      withWallets distribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.contract minDurationStartProtocolParams
        frData <- withKeyWallet bob $ Create.contract protocol (mkFundraisingParams 80 (mkFundraisingDuration 0 0 1))
        liftAff $ delay $ Milliseconds 60000.0 -- 1 min
        result <- try $ withKeyWallet alice $ void $ Donate.contract frData (fromInt 20)
        let errMsg = "fundraising time is over"
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should successfully donate on 1 minute duration funsrising" do
      withWallets distribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.contract minDurationStartProtocolParams
        frData <- withKeyWallet bob $ Create.contract protocol (mkFundraisingParams 80 (mkFundraisingDuration 0 0 1))
        withKeyWallet bob $ void $ Donate.contract frData (fromInt 50)

incorrectFundrisingData :: Protocol -> Contract FundraisingData
incorrectFundrisingData protocol = do
  tn <- Helpers.runMkTokenName "FundraisingThreadToken"
  pure $ FundraisingData { protocol: protocol, frThreadTokenCurrency: Value.adaSymbol, frThreadTokenName: tn }

minDurationStartProtocolParams :: ProtocolConfigParams
minDurationStartProtocolParams = ProtocolConfigParams
  { minAmountParam: BigInt.fromInt 2_000_000
  , maxAmountParam: BigInt.fromInt 100_000_000
  , minDurationParam: BigInt.fromInt 1 -- minutes
  , maxDurationParam: BigInt.fromInt 250 -- minutes
  , protocolFeeParam: BigInt.fromInt 5 -- percentage
  }
