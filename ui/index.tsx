import ReactDOM from 'react-dom/client';

const a = await import ('@offchain/Common.ConnectWallet')

const root = ReactDOM.createRoot(document.getElementById('root'))

const App = () => {
  return (
    <div>
      <h1>Offchain integration</h1>
      <button onClick={a.runConnectWallet}>Connect wallet</button>
    </div>
  )
}

root.render(<App/>)
