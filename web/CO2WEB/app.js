/***
 * app main
 * @type {*}
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

var express = require('express');
var path = require('path');
var httplogger = require('morgan');
var request =require("request");
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
var util = require('util');

var visualizeWorld =require("./routes/visualizeWorld.js");
var visualizeBMS =require("./routes/visualizeBms.js");
var visualizeSemakau =require("./routes/visualizeSemakau.js");
var visualizeJurong =require("./routes/visualizeJurong.js");
var showCO2 = require("./routes/showCO2.js");
var config = require("./config.js");
var bmsplot= require("./routes/bmsplot.js");
var bmsTemp = require("./routes/bmsNodeTemp");
//var PPCO2 = require("./routes/powerplantCO2");
//var ppMap = require('./routes/mapPowerPlant')
var bmsData = require('./agents/GetBmsData')
var registerer= require("./agents/register2DataChange");
var setBMSWatcher = require("./agents/setBMSWatcher");
var registerUrl = config.bmsUrlPath;
var myUrl = config.myUrlPath;


var app = express();
var port = config.port;
process.env.UV_THREADPOOL_SIZE = 128;

var BMSWatcher = require('./agents/setBMSWatcher');


app.set('view engine', 'pug');
app.use(httplogger('dev'));

function setHeader(res, mpath){
  logger.debug("path"+ mpath);
  res.setHeader("Content-Type","text/xml");
  res.setHeader("Content-Disposition","inline");
    logger.debug("SEtting headers");


}app.use(bodyParser.text({ type: 'application/json' }));

/*serve static file***/
app.use(express.static(path.join(__dirname, 'public')));
//app.use(express.static(path.join(__dirname, 'ROOT'), {'setHeaders': setHeader}));
app.use('/visualizeWorld', visualizeWorld);
app.use('/visualizeBMS', visualizeBMS);
app.use('/visualizeSemakau', visualizeSemakau);
app.use('/visualizeJurong', visualizeJurong);
//app.use('/PowerPlantCO2',  PPCO2);
//app.use('/ppmap', ppMap);

app.use('/JurongIsland.owl/showCO2', showCO2);
app.use('/JPS_KB_CARES_Lab_Node/FH-01.owl', bmsTemp);
app.use("/bmsplot", bmsplot);
app.get('/', function (req, res) {
	        res.sendFile(path.join(__dirname, 'views/index.html'));

});

/*posting to dataObserve to get orginal data & register for future data change*/
var dataCopy = null;


var http = require('http').Server(app);
var io = require('socket.io')(http);

/*future data change will be post to this route*/

/**
app.post("/change", function (req, res) {//data change of other nodes will be post to here
    //retreive changed data//do whatever you need to do with this data
    //now we only record it down
    if(req.body) {
    dataCopy = req.body;
    logger.debug(req.body);
    io.emit("update", req.body);
        res.status(200).send("success");
} else {
  logger.debug("Receive empty data");
          res.status(400).send("empty req body: should contain data");
}
});
***/

var ev= BMSWatcher();

//Save a datacopy for initialization
ev.on('change', function (data) {
//TODO: uri to diskloc
    logger.debug("update event: "+" on "+data.uri);
    logger.debug(data)
    io.to(data.uri).emit("update", data);

})

/*socket io***/
//TODO: multiple nodes may post to here, need a map of dataCopy, name as key
//TODO: else, post again for initial data(seperate it with register)
io.on('connection', function(socket){


       // registerer.register(registerUrl, myUrl, function (err, initialData) {//register to changing-data node
       //     if(err){
         //       logger.debug(err);
          //      return;//TODO: err handling
          //  }

/**
        bmsData(function (err, initialData) {
            //get initial by db access
            socket.emit("initial",initialData);
        })
**/
socket.on('join', function (uriSubscribeList) {
    //May be do some authorization
    let sl = JSON.parse(uriSubscribeList);
    sl.forEach(function (uri2Sub) {
        let diskLoc = uri2Sub.replace("http://www.theworldavatar.com", config.bmsFolder)
            .replace("http://www.jparksimulator.com", config.bmsFolder);

        diskLoc = path.normalize(diskLoc)
        logger.debug(socket.id, "joined", diskLoc);
        socket.join(diskLoc);
        logger.debug(config.bmsplotnode)
        logger.debug(socket.rooms)
        if(diskLoc === config.bmsplotnode){
            bmsData(config.bmsplotnode, function (err, initialData) {
                //get initial by db access
                logger.debug("send initial data");
                socket.emit("initial",initialData);
            })
        }

    })



});
    socket.on('leave', function (uriSubscribe) {
        //May be do some authorization
        socket.leave(uriSubscribe);
        logger.debug(socket.id, "left", uriSubscribe);
    });

    logger.debug('a user connected');

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
  res.render('error', {error: err});
});
/********************/


/***
http.on('close', function () {
   //now deregister it
    registerer.deregister(registerUrl, myUrl, function (err, result) {
          //server is close down, no way to put this msg to anyone, just print it out
          if(err){
              logger.debug(err);
          }

          logger.debug(result);
      })


});
***/
http.listen(port, function () {
  logger.debug('Server listening on port 3000');
});

module.exports = http;
