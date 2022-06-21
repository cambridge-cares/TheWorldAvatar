/**import***/
var express = require('express');
var path = require('path');
var logger = require('morgan');
var bodyParser = require('body-parser');
/**router modules***/
var data = require("./routes/data.js");
var query = require("./routes/query.js");
var sim = require("./routes/simulation.js");
var visual = require("./routes/visualization.js");
/**express&io server***/
var app = express();
var http = require('http').Server(app);
var io = require('socket.io')(http);

/**view set up***/
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'jade');

/**middleware***/

app.use(logger('dev'));
app.use(bodyParser.json());

//app.use(express.static(path.join(__dirname, 'public')));

//Tested socket io client
app.get('/', function (req, res) {
  res.sendFile(__dirname +"/views/test.html");
});

//serve sub-routes
app.use('/data', data);
app.use('/query', query);
app.use('/sim', sim);
app.use('/visual', visual);


//test post data, to be deleted
app.get("/test", function (req, res) {
  res.sendFile(__dirname +"/views/testUpdateData.html");

});

/***socket io***/
io.on('connection', function(socket){
  console.log('a user connected');
});

var callback = function (data) {
  console.log("data changed");
  io.emit('update', data);
};

var eventController = (require("./util/changeListener"))();

var eventCtrl = eventController.eventCtrl;
eventCtrl.on("change",callback);


// catch 404 and forward to error handler
app.use(function(req, res, next) {
  var err = new Error('Not Found');
  err.status = 404;
  next(err);
});

// error handler
app.use(function(err, req, res, next) {
  // set locals, only providing error in development
  res.locals.message = err.message;
  res.locals.error = req.app.get('env') === 'development' ? err : {};

  // render the error page
  res.status(err.status || 500);
  res.render('error');
});

var server = http.listen(2333, function () {
  console.log('IO Server listening on port 2333');
});
module.exports = app;
