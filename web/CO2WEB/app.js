var express = require('express');
var path = require('path');
var logger = require('morgan');
var request =require("request");
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
var util = require('util');

var visualize =require("./routes/visualize.js");
var showCO2 = require("./routes/showCO2.js");
var config = require("./config.js");
var bmsplot= require("./routes/bmsplot.js");
var bmsTemp = require("./routes/bmsNodeTemp");


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
app.use('/JPS_KB_CARES_Lab_Node/FH-01.owl', bmsTemp);
app.use("/bmsplot", bmsplot);

app.get('/', function (req, res) {
	        res.sendFile(path.join(__dirname, 'views/index.html'));

});

/*posting to dataObserve to get orginal data & register for future data change*/
var dataCopy = null;
function register(callback) {
    request.post({url: config.bmsUrl ,body: JSON.stringify({url:config.myUrl, getInitData:true}),headers: {
        'content-type': 'application/json'
    }}, function (err,res ,body) {

        console.log("posting to dataObserve localhost 2000");

        if(err){
            console.log(err);
            callback(err);
            return;
        }
        console.log("intial body");
        console.log(body);
        dataCopy = body;
        callback(null, body);
    });

};



var http = require('http').Server(app);
var io = require('socket.io')(http);

/*future data change will be post to this route*/

app.use(bodyParser.text({ type: 'text/xml' }));

app.post("/change", function (req, res) {
    //retreive changed data
    //do whatever you need to do with this data
    //now we only record it down

    console.log("update body");
    console.log(util.inspect(req.body));

//console.log(req.header('content-type'));
//TODO: socket, send this info to all clients
    dataCopy = req.body;
    io.emit("update", req.body);
    res.status(200).send("success");
});



/*socket io***/
io.on('connection', function(socket){
    if(dataCopy === null){
        register(function (err, initialData) {
            if(err){
                console.log(err);
                return;//TODO: err handling
            }

            socket.emit("initial",initialData);
            dataCopy = initialData;
            console.log('a user connected');

        })


    } else {
        socket.emit("initial", dataCopy);
        console.log('a user connected');
    }
});

/*err handling*/
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





module.exports = app;

http.on('close', function () {
   //now deregister it


});

http.listen(port, function () {
  console.log('Server listening on port 3000');
});
