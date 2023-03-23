import { useState } from 'react';
import ReactDOM from 'react-dom/client';

const a = await import('@offchain/Scaffold.Main');

// eslint-disable-next-line @typescript-eslint/no-non-null-assertion
const root = ReactDOM.createRoot(document.getElementById('root')!);

const App = () => {

  const startProtocolParams = {
    minAmountParam: 50000000,
    maxAmountParam: 1000000000,
    minDurationParam: 1,
    maxDurationParam: 90,
    protocolFeeParam: 10,
  };
  const updatedParams = {
    minAmountParam: 50000000,
    maxAmountParam: 1000000000,
    minDurationParam: 1,
    maxDurationParam: 90,
    protocolFeeParam: 9,
  };
  const createFundraisingParams = {
    description: 'Donate to feed stray cats',
    amount: 100,
    duration: 1
  }

  const [protocol, setProtocol] = useState();
  const [fundraisingData, setFundraisingData] = useState ();
 
  const onStartProtocolComplete = completedProtocol => {
    console.log(completedProtocol);
    setProtocol(completedProtocol);
  };

  const onUpdateProtocolComplete = something => {
    console.log('update success');
  };

  const onCreateFundraisingComplete = createdFundraisingResponse => {
    setFundraisingData(createdFundraisingResponse);
   
  };

  const onStartProtocolClick = () => {
    a.main.value0.startProtocol(onStartProtocolComplete)(console.log)(
      startProtocolParams
    )();
  };
  const onUpdateProtocolClick = () => {
    a.main.value0.updateProtocol(onUpdateProtocolComplete)(console.log)(
      protocol
    )(updatedParams)();
  };
  const onCloseProtocolClick = () => {
    a.main.value0.closeProtocol(console.log)(console.log)(protocol)();
  };

  const onCreateFundraisingClick = () => {
    a.main.value0.createFundraising(onCreateFundraisingComplete)(console.log)(
      protocol
    )(createFundraisingParams)();
  };

  const onDonate = () => {   
    a.main.value0.donate(console.log)(console.log)(fundraisingData)(100_000_000)();
  };

  const onReceiveFunds = () => {
    a.main.value0.receiveFunds(console.log)(console.log)(fundraisingData)();
  }
  
  const onGetAllFundraising = () => {
    a.main.value0.getAllFundraisings(console.log)(console.log)(protocol)();
  };

  const onGetUserRelatedFundraisings = () => {
    a.main.value0.getUserRelatedFundraisings(console.log)(console.log)(protocol)();
  };

  const onConnectWallet = () => {
    a.main.value0.connectWallet(console.log)(console.log)();
  };

  return (
    <div>
      <h1>Offchain integration</h1>
      <button onClick={onConnectWallet}>Connect wallet</button>
      <button onClick={onStartProtocolClick}>Start Protocol</button>
      <button onClick={onUpdateProtocolClick}>Update Protocol</button>
      <button onClick={onCloseProtocolClick}>Close Protocol</button>
      <button onClick={onCreateFundraisingClick}>Create fundraising</button>
      <button onClick={onDonate}>Donate 100 Ada</button>
      <button onClick={onReceiveFunds}>Receive funds</button>
      <button onClick={onGetAllFundraising}>Get All Fundraisings</button>
      <button onClick={onGetUserRelatedFundraisings}>Get User related Fundraisings</button>
    </div>
  );
};

root.render(<App />);
