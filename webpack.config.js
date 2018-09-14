// Inside of webpack.config.js:
const {InjectManifest, GenerateSW} = require('workbox-webpack-plugin');
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');
const CompressionPlugin = require('compression-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');



const mode = 'development';//'production';

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

const  minimizer = mode !== 'production' ? [] : [
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
      ];

module.exports = {
  // Other webpack config...
  mode: mode,
  entry: ["./alarm.mp3", "./index.html", "./index.js"],
  devtool: mode === 'development' ? 'source-map' : undefined,
  module: {
    rules: [
        {
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: {
        loader: 'elm-webpack-loader',
        options: {
            pathToElm: 'node_modules/.bin/elm',
            optimize: mode === 'production',
            debug: mode !== 'production',
            cwd: "."
        }
      }
    },
    {
        test: /(\.mp3$)|(.\html)/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
        { 
            loader: 'file-loader',
            options: {
              name: '[name].[ext]'
            }
        }
        ]
    }
    ]
  },
  plugins: [
    new GenerateSW({
        swDest: "./sw.js",
        runtimeCaching: [{
            urlPattern: new RegExp('^http.*'),
            handler: 'cacheFirst',
            options: {
                cacheableResponse: {
                  statuses: [0, 200, 304]
                },
                plugins: [{requestWillFetch: () => console.log("will-fetch")}]
              }
         
        }]
    }),
    new CompressionPlugin(),
    new CopyWebpackPlugin([
    ])
  ],
  optimization: {
    minimizer: minimizer
  }
};


