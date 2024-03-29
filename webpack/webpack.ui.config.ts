import HtmlWebpackPlugin from 'html-webpack-plugin';
import MiniCssExtractPlugin from 'mini-css-extract-plugin';
import NodePolyfillWebpackPlugin from 'node-polyfill-webpack-plugin';
import path from 'path';
import TsconfigPathsPlugin from 'tsconfig-paths-webpack-plugin';
import {
  type Configuration,
  ContextReplacementPlugin,
  DefinePlugin,
  ProvidePlugin,
} from 'webpack';
import 'webpack-dev-server';

const isProduction = process.env.NODE_ENV === 'production';

const stylesHandler = isProduction
  ? MiniCssExtractPlugin.loader
  : 'style-loader';

const config: Configuration = {
  mode: isProduction ? 'production' : 'development',
  entry: path.resolve(__dirname, '../ui/index.tsx'),
  target: 'web',
  devtool: 'eval-source-map',
  output: {
    path: path.resolve(__dirname, 'dist'),
  },

  stats: { errorDetails: true },

  experiments: {
    syncWebAssembly: true,
    topLevelAwait: true,
  },

  devServer: {
    open: false,
    host: 'localhost',
    hot: true,
    port: 4008,
    client: {
      overlay: {
        warnings: false,
        errors: true,
      },
    },
  },
  plugins: [
    new DefinePlugin({
      BROWSER_RUNTIME: !!process.env.BROWSER_RUNTIME,
    }),
    new NodePolyfillWebpackPlugin(),
    new HtmlWebpackPlugin({
      template: 'index.html',
    }),
    new ProvidePlugin({
      Buffer: ['buffer', 'Buffer'],
    }),
    new ContextReplacementPlugin(/cardano-serialization-lib-browser/),
    new ContextReplacementPlugin(/cardano-serialization-lib-nodejs/),
  ].filter(Boolean),

  module: {
    rules: [
      {
        test: /\.tsx?$/,
        exclude: /node_modules/,
        use: [
          {
            loader: 'babel-loader',
            options: {
              cacheDirectory: true,
              presets: [
                '@babel/preset-env',
                ['@babel/preset-react', { runtime: 'automatic' }],
                '@babel/preset-typescript',
              ],
              plugins: ['@babel/plugin-transform-runtime'],
            },
          },
        ],
      },
      {
        test: /\.plutus$/i,
        type: 'asset/source',
      },
      {
        test: /\.css$/i,
        use: [stylesHandler, 'css-loader', 'postcss-loader'],
      },
      {
        test: /\.(eot|svg|ttf|woff|woff2|png|jpg|gif)$/i,
        type: 'asset',
      },
    ],
  },
  resolve: {
    modules: [process.env.NODE_PATH ?? ' '],
    extensions: ['.tsx', '.ts', '.jsx', '.js'],
    fallback: {
      buffer: require.resolve('buffer/'),
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
      Scripts: path.resolve(__dirname, '../scripts'),
    },
    plugins: [
      new TsconfigPathsPlugin({
        configFile: './tsconfig.json',
        extensions: ['.tsx', '.ts', '.jsx', '.js'],
      }),
    ],
  },
};

export default config;
