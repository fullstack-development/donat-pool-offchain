import { useState } from 'react';
import ReactDOM from 'react-dom/client';

const a = await import('@offchain/Scaffold.Main');

// eslint-disable-next-line @typescript-eslint/no-non-null-assertion
const root = ReactDOM.createRoot(document.getElementById('root')!);

const App = () => {
  const startProtocolParams = {
    minAmountParam: 50000000,
    maxAmountParam: 1000000000,
    minDurationParam: 100,
    maxDurationParam: 1000,
    protocolFeeParam: 10,
  };
  const updatedParams = {
    minAmountParam: 50000000,
    maxAmountParam: 1000000000,
    minDurationParam: 100,
    maxDurationParam: 1000,
    protocolFeeParam: 9,
  };
  const [protocol, setProtocol] = useState();

  const onStartProtocolComplete = completedProtocol => {
    setProtocol(completedProtocol);
  };

  const onUpdateProtocolComplete = something => {
    console.log('update success');
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

  return (
    <div>
      <h1>Offchain integration</h1>
      <button onClick={onStartProtocolClick}>Start Protocol</button>
      <button onClick={onUpdateProtocolClick}>Update Protocol</button>
    </div>
  );
};

root.render(<App />);
