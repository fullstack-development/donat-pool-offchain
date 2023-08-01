"use strict";

const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const NodePolyfillPlugin = require("node-polyfill-webpack-plugin");

module.exports = (env, argv) => ({
  mode: argv.mode === 'development' ? 'development' : 'production',

  experiments: {
    asyncWebAssembly: false,
    layers: false,
    lazyCompilation: false,
    syncWebAssembly: true,
    topLevelAwait: true,
  },

  devtool: argv.mode === 'development' ? "eval-source-map" : false,

  stats: { errorDetails: true },

  devServer: {
    port: 4008,
  },

  // we can add more entrypoints as needed
  entry: "./index.js",

  output: {
    path: path.resolve(__dirname, "dist"),
    filename: 'index.js'
  },

  module: {
    rules: [
      {
        test: /\.(png|jpg|gif)$/i,
        type: "asset",
      },
      {
        test: /\.plutus$/i,
        type: "asset/source",
      },
    ],
  },

  resolve: {
    modules: [process.env.NODE_PATH],
    extensions: [".js"],
    fallback: {
      buffer: require.resolve("buffer/"),
      http: false,
      url: false,
      stream: false,
      crypto: false,
      https: false,
      net: false,
      tls: false,
      zlib: false,
      os: false,
      path: false,
      fs: false,
      readline: false,
      child_process: false,
    },
    alias: {
      // You should update this path to the location of your compiled scripts,
      // relative to `webpack.config.js`
      Scripts: path.resolve(__dirname, "scripts"),
    },
  },

  plugins: [
    new webpack.DefinePlugin({
      BROWSER_RUNTIME: !!process.env.BROWSER_RUNTIME,
    }),
    new NodePolyfillPlugin(),
    new webpack.LoaderOptionsPlugin({
      debug: true,
    }),
    argv.mode === 'development'  && new HtmlWebpackPlugin({
      title: "ctl-scaffold",
      template: "./index.html",
      inject: 'body',
    }),
    new webpack.ProvidePlugin({
      Buffer: ["buffer", "Buffer"],
    }),
    new webpack.ContextReplacementPlugin(/cardano-serialization-lib-browser/),
    new webpack.ContextReplacementPlugin(/cardano-serialization-lib-nodejs/),
  ].filter(Boolean),
});
