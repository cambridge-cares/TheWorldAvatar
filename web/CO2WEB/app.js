/***
 * app main
 * @type {*}
 */
var log4js = require('log4js');
log4js.configure({
  appenders: { defaultLogger: { type: 'dateFile', filename: 'C:/jps/temp/logs/js-server.log' , pattern: '.yyyy-MM-dd-hh', compress: true } },
  categories: { default: { appenders: ['defaultLogger'], level: 'error' } }
});
var logger = log4js.getLogger('defaultLogger');
logger.level = 'debug';

var express = require('express');
var path = require('path');
var httplogger = require('morgan');
var request =require("request");
var bodyParser = require('body-parser');
var util = require('util');
var config = require("./config.js");


var visualizeWorld =require("./routes/visualizeWorld.js");
var visualizeBMS =require("./routes/visualizeBms.js");
var visualizeSemakau =require("./routes/visualizeSemakau.js");
var visualizeJurong =require("./routes/visualizeJurong.js");
var visualizeOntoEN = require("./routes/visualizeOntoEN.js");
var visualizeOntoChem = require("./routes/visualizeOntoChem.js");
var visualizeAgent = require("./routes/visualizeAgent.js");
var visualizeOntokin= require("./routes/visualizeOntokin.js");


 var showCO2 = require("./routes/showCO2");
var bmsplot= require("./routes/plotBMSCached.js");
var getAttrList =require("./routes/getAttrList");
var getSpecAttr =require("./routes/getSpecificLiteralAttrCached");
//var MAU = require("./routes/runMAU")
var MAUPlot = require("./routes/plotMAU")
var HW =require("./routes/runHeatWasteNetworkMap")
//var PPCO2 = require("./routes/powerplantCO2Cached");
var PPCO2 = require("./routes/powerplantCO2");

var ppMap = require('./routes/mapPowerPlant');

var semakauMap = require("./routes/mapSemakau")
//var b3Map = require("./routes/mapB3")
var b2Map = require("./routes/mapB2")
var ppalt = require("./routes/mapPPAlt")

var literalData = require('./agents/GetLiteralData');
var visualizeOntoEN = require("./routes/visualizeOntoEN.js");
var getChildrenSingle = require('./routes/GetChildrenSingle');

var BMSWatcher = require('./agents/setBMSWatcher');
var agentWatcher = require('./agents/msgFace');

var app = express();
var port = config.port;
process.env.UV_THREADPOOL_SIZE = 128;


app.set('view engine', 'pug');
app.use(httplogger('dev'));

function setHeader(res, mpath){
  logger.debug("path"+ mpath);
  res.setHeader("Content-Type","text/xml");
  //res.setHeader('Access-Control-Allow-Origin', 'http://www.theworldavatar.com:80');

  res.setHeader("Content-Disposition","inline");
    logger.debug("SEtting headers");
}
/*body parser*/
app.use(bodyParser.text({ type: 'application/json' }));

/*serve static file***/
app.use(express.static(path.join(__dirname, 'public')));
app.use(express.static(path.join(__dirname, 'ROOT'), {'setHeaders': setHeader}));

function acHeader(res){
	  res.setHeader('Access-Control-Allow-Origin', 'http://www.theworldavatar.com:80');

}
app.use('/visualizeAgent', visualizeAgent);

app.use('/visualizeWorld', visualizeWorld);
app.use('/visualizeBMS', visualizeBMS);
app.use('/visualizeSemakau', visualizeSemakau);
app.use('/visualizeJurong', visualizeJurong);
app.use('/PowerPlantCO2',  PPCO2);
app.use('/semakaumap', semakauMap);
app.use('/ppalt', ppalt);
app.use('/JurongIsland.owl/showCO2', showCO2);
app.use('/visualizeOntoEN',visualizeOntoEN);
app.use('/visualizeOntoChem',visualizeOntoChem);
app.use('/visualizeOntokin',visualizeOntokin);

app.use('/getChildrenSingle',getChildrenSingle);

app.use("/bmsplot", bmsplot);

app.use('/ppmap', ppMap);


app.use('/b2map', b2Map)
 app.use("/hw", HW);

app.use("/mauplot", MAUPlot);
app.use("/getAttrList", getAttrList);
app.use("/getSpecAttr", getSpecAttr);
//app.use("/MAU", MAU);



/*posting to dataObserve to get orginal data & register for future data change*/



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
var watcherReturn = BMSWatcher();
var ev= watcherReturn.watchEvent;
var bmsWatcher = watcherReturn.bmsWatcher;
agentWatcher.init(io);
//When any change happened to the file system
ev.on('change', function (data) {
    logger.debug("update event: "+" on "+data.uri+"_nodata");
	    //let rooms = io.sockets.adapter.rooms;
   //logger.debug(rooms[path.normalize(data.uri)].sockets);
    //update direct clients
    io.to(path.normalize(data.uri)+"_nodata").emit("update", {uri:data.uri, filename:data.filename});
    io.to(path.normalize(data.uri)+"_data").emit("update", data);
})

/*socket io***/

io.on('connection', function(socket){
    

socket.on('join', function (uriSubscribeList) {
    //May be do some authorization

    console.log('client join')
    let sl = JSON.parse(uriSubscribeList);
    logger.debug(sl)
    sl.forEach(function (uri2Sub) {
        let diskLoc = uri2Sub.uri.replace("http://www.theworldavatar.com", config.root)
            .replace("http://www.jparksimulator.com", config.root);


        let affix = uri2Sub.withData? "_data" :"_nodata";
        diskLoc = path.normalize(diskLoc)
        socket.join(diskLoc+affix);
		      // console.log(socket.id, "joined", diskLoc+affix);

		

        //TODO:check client legnth first, if 0 ,first join, ask to register for data


        if(uri2Sub.withData){
            var clients = io.sockets.adapter.rooms[diskLoc+affix].sockets;

//to get the number of clients
            var numClients = (typeof clients !== 'undefined') ? Object.keys(clients).length : 0;
            logger.debug("number of clients in room: "+numClients);
            if (numClients < 2 ){//first join for data, register for data now
                logger.debug("first client for this node ,register for data change")
                bmsWatcher.register(diskLoc,"worldnode", true);
            }


                console.log(diskLoc);
                literalData( function (err, initialData) {
                    //get initial by db access
                    logger.debug("send initial data");
                    socket.emit("initial",initialData);
                }, diskLoc);


        }
    })
    logger.debug("@@@@@@@@@@@@@@@@")
    logger.debug(io.sockets.adapter.rooms)
});
    socket.on('leave', function (uriSubscribeList) {
        //May be do some authorization
        let sl = JSON.parse(uriSubscribeList);
        sl.forEach(function (uri) {
			        let diskLoc = uri.replace("http://www.theworldavatar.com", config.root)
            .replace("http://www.jparksimulator.com", config.root);
			diskLoc = path.normalize(diskLoc)
            socket.leave(diskLoc+"_data");
            socket.leave(diskLoc+"_nodata");
            logger.debug(socket.id, "left", diskLoc);
            //TODO: deregister if a room owns no client?

        })

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


http.listen(port, function () {
  console.log('Server listening on port '+port);
});

module.exports = http;
