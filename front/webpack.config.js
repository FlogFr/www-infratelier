const path = require("path");
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const CompressionPlugin = require('compression-webpack-plugin');

var PROD = JSON.parse(process.env.PROD_ENV || '0');

module.exports = {
  entry: {
    index: "./src/main.js",
    form: "./src/FormPage.jsx",
  },
  output: {
    filename: "[name].bundle.js",
    path: path.join(__dirname, "/dist"),
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        exclude: /node_modules/,
        use: [{
          loader: "babel-loader"
        }]
      }, {
        test: /\.scss?$/,
        exclude: /node_modules/,
        use: PROD ? [
          MiniCssExtractPlugin.loader,
          'css-loader',
          'sass-loader',
        ] : [
          {
            loader: MiniCssExtractPlugin.loader,
            options: {
              // you can specify a publicPath here
              // by default it uses publicPath in webpackOptions.output
              publicPath: '../',
              hmr: process.env.NODE_ENV === 'development',
            },
          },
          'css-loader',
          'sass-loader',
        ]
      }, 
    ]
  },
  plugins: PROD ? [
    new MiniCssExtractPlugin(
      'css/[name].css'
    ),
    new CompressionPlugin({
      test: /\.js(\?.*)?$/i,
    }),
    new OptimizeCssAssetsPlugin({
      assetNameRegExp: /\.css$/,
      cssProcessorOptions: { discardComments: { removeAll: true } }
    }),
  ] : [
    new MiniCssExtractPlugin(
      'css/[name].css'
    ),
  ],
  optimization: PROD ? {
    minimizer: [
      new UglifyJsPlugin()
    ],
  } : {}
};
