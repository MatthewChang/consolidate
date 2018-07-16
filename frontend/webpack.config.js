module.exports = {
  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: {
        loader: 'elm-webpack-loader',
        options: {
          verbose: true,
          warn: true,
          debug: true
        }
      }
    },{
        test: /\.css$/,
        use: [
          { loader: "style-loader" },
          { loader: "css-loader" }
        ]
      }, {
        test: /\.scss$/,
        use: [
          { loader: "style-loader" },
          { loader: "css-loader" },
          { loader: "sass-loader" }
        ]
      }
    ]
  },
  entry: {
    app: __dirname + '/src/index.js',
  },
  output: {
    path: '/Users/matthewchang/music_server/music_server/backend/static/static', // `dist` is the destination
    publicPath: "/assets/",
    filename: 'bundle.js',
  },
};
