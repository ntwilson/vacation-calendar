const path = require('path');

module.exports = {
  entry: './src/Vacate/Backend/entry.js',
  output: {
    path: path.resolve(__dirname, 'release'),
  },
  mode: 'development',
};