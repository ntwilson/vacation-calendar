const path = require('path');

module.exports = {
  entry: './src/Vacate/Frontend/entry.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
  },
  mode: 'development',
};