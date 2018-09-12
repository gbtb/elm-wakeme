// Inside of webpack.config.js:
const WorkboxPlugin = require('workbox-webpack-plugin');
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');
const CompressionPlugin = require('compression-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

// Enable users to turn on dead code elimination.
const deadCodeElimination =
     {
        dead_code: true,
        pure_funcs: [
          '_elm_lang$core$Native_Utils.update',
          'A2',
          'A3',
          'A4',
          'A5',
          'A6',
          'A7',
          'A8',
          'A9',
          'F2',
          'F3',
          'F4',
          'F5',
          'F6',
          'F7',
          'F8',
          'F9'
        ]
      };

module.exports = {
  // Other webpack config...
  mode: "production",
  entry: "./index.js",
  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: {
        loader: 'elm-webpack-loader',
        options: {
            pathToElm: 'node_modules/.bin/elm',
            optimize: true,
            cwd: "."
        }
      }
    }]
  },
  plugins: [
    new WorkboxPlugin.GenerateSW(),
    new CompressionPlugin(),
    new CopyWebpackPlugin([{
        from: './*.html'
      }])
  ],
  optimization: {
    minimizer: [
        new UglifyJsPlugin({
            uglifyOptions: {
              keep_fargs: false,
              compress: Object.assign(
                {},
                {
                  warnings: false,
                  comparisons: true,
                  unsafe: true,
                  unsafe_comps: true,
                  pure_getters: true
                },
                deadCodeElimination
              ),
              output: {
                comments: false,
                // Turned on because emoji and regex is not minified properly using default
                // https://github.com/facebook/create-react-app/issues/2488
                ascii_only: true
              }
            },
            // Use multi-process parallel running to improve the build speed
            // Default number of concurrent runs: os.cpus().length - 1
            parallel: true,
            // Enable file caching
            sourceMap: false
          }),
          new UglifyJsPlugin({
              uglifyOptions: {
                  mangle: false
              },
              // Use multi-process parallel running to improve the build speed
              // Default number of concurrent runs: os.cpus().length - 1
              parallel: true,
              // Enable file caching
              sourceMap: false
            })
      ],
  }
};
