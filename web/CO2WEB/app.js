var express = require('express');
var path = require('path');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
var rdfParser = require(__dirname+"/rdfParser.js");
var visualize =require("./routes/visualize.js");

var app = express();


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

app.get('/', function (req, res) {
	        res.sendFile(path.join(__dirname, 'views/index.html'));

});
app.get('/JurongIsland.owl/showCO2', function (req, res) {

  let opts = {//format opts to feed in rdfParser
    fileUrl : __dirname+"/ROOT/JurongIsland/JurongIsland.owl",
    uri :'http://www.jparksimulator.com/JurongIsland.owl'


  };

  let node ="http://www.theworldavatar.com/JurongIsland.owl#V_CO2_Jurong"; // individual
  let property = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue";//property

  let parser = rdfParser(opts);
  let result = parser.findValue(node, property);//search in parsed graph for this property of this note

  result = parseFloat(result).toFixed(4);//format result into float like 1.0000
  res.render('co2', { co2Value: result }); //render the view with this value


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
var server = app.listen(82, function () {
  console.log('Server listening on port 82');
});
