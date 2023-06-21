import { useState } from 'react';
import ReactDOM from 'react-dom/client';

const a = await import('@offchain/Scaffold.Main');

// eslint-disable-next-line @typescript-eslint/no-non-null-assertion
const root = ReactDOM.createRoot(document.getElementById('root')!);

const App = () => {

  const startProtocolParams = {
    minAmountParam: 50000000,
    maxAmountParam: 1000000000,
    minDurationParam: 5,  // 5 minutes
    maxDurationParam: 86400,  // 60 days
    protocolFeeParam: 10,
  };
  const updatedParams = {
    minAmountParam: 50000000,
    maxAmountParam: 1000000000,
    minDurationParam: 5,  // 5 minutes
    maxDurationParam: 86400,  // 60 days
    protocolFeeParam: 9,
  };

  const fundraisingDuration = {
    days: 0,
    hours: 0,
    minutes: 6
  };
  
  const createFundraisingParams = {
    description: 'Donate to feed stray cats',
    amount: 200,
    duration: fundraisingDuration
  };

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
    const frThreadTokenCurrency = createdFundraisingResponse && createdFundraisingResponse.threadTokenCurrency ? createdFundraisingResponse.threadTokenCurrency : null;
    const frThreadTokenName = createdFundraisingResponse && createdFundraisingResponse.threadTokenName ? createdFundraisingResponse.threadTokenName : null 
    const frData = {
      frThreadTokenCurrency: frThreadTokenCurrency,
      frThreadTokenName: frThreadTokenName
    };
    
    setFundraisingData(frData);
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
    a.main.value0.donate(console.log)(console.log)(protocol)(fundraisingData)(100)();
  };

  const onReceiveFunds = () => {
    a.main.value0.receiveFunds(console.log)(console.log)(protocol)(fundraisingData)();
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

  const onGetAppInfo = () => {
    a.main.value0.getAppInfo(console.log)(console.log)(protocol)();
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
      <button onClick={onGetAppInfo}>Get app info</button>
    </div>
  );
};

root.render(<App />);
