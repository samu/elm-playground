var jsonServer = require('json-server')

// Returns an Express server
var server = jsonServer.create()

// Allow CORS
server.use(function(req, res, next) {
  res.header("Access-Control-Allow-Origin", "*");
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
  next();
});

server.get('/', function (req, res) {
  res.send({
    abc: "this is a test",
    more: {
      "sane array": [1, 2, 3, 4, 5],
      "crazy array": [1, "2", 3, 4],
      nested: {
        i_told_you: {
          "yes very nested": "YES!"
        }
      }
    }
  })
})

console.log('Listening at 3000')
server.listen(3000)
