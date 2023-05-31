const path = require("path");
const common = require("./webpack.common");
const { merge } = require("webpack-merge");
const TerserPlugin = require("terser-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const CssMinimizerPlugin = require("css-minimizer-webpack-plugin");

module.exports = merge(common, {
  mode: 'production',
  output: {
    filename: "[name].[contenthash].bundle.js",
    path: path.resolve(__dirname, 'dist'),
    clean: true,
    sourcePrefix: '',
  },
  optimization: {
    splitChunks: {
      chunks: 'all',
    },
    minimizer: [
      `...`, // extends existing plugins
      new CssMinimizerPlugin(),
      new HtmlWebpackPlugin({
        template: "src/index.html",
        minify: {
          removeAttributeQuotes: true,
          collapseWhitespace: true,
          removeComments: true
        }
      })
    ]
  },
  module: {
    rules: [
      {
        test: /\.css$/i,
        use: [MiniCssExtractPlugin.loader, "css-loader"],
      },
    ],
  },
  plugins: [new MiniCssExtractPlugin({ filename: "[name].[contenthash].css" })],
});