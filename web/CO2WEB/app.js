var express = require('express');
var path = require('path');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
var visualize =require("./routes/visualize.js");
var showCO2 = require("./routes/showCO2.js");
var config = require("./config.js");
var app = express();
var port = config.port;
process.env.UV_THREADPOOL_SIZE = 128;

// uncomment after placing your favicon in /public
//app.use(favicon(path.join(__dirname, 'public', 'favicon.ico')));
app.set('view engine', 'pug');

app.use(logger('dev'));

function setHeader(res, mpath){
  console.log("path"+ mpath);
  res.setHeader("Content-Type","text/xml");
  res.setHeader("Content-Disposition","inline");
    console.log("SEtting headers");


}
//serve static file
app.use(express.static(path.join(__dirname, 'public')));
//app.use(express.static(path.join(__dirname, 'ROOT'), {'setHeaders': setHeader}));
app.use('/visualize', visualize);
app.use('/JurongIsland.owl/showCO2', showCO2);

app.get('/', function (req, res) {
	        res.sendFile(path.join(__dirname, 'views/index.html'));

});



/**

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
**/
module.exports = app;

var server = app.listen(port, function () {
  console.log('Server listening on port 3000');
});
