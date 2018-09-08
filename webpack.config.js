// Inside of webpack.config.js:
const WorkboxPlugin = require('workbox-webpack-plugin');

module.exports = {
  // Other webpack config...

  plugins: [
    // Other plugins...

    new WorkboxPlugin.GenerateSW()
  ]
};
