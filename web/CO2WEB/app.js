/***
 * app main
 * @type {*}
 */
const log4js = require('log4js');
log4js.configure({
  appenders: { defaultLogger: { type: 'file', filename: 'C:/jps/temp/logs/js-server.log' } },
  categories: { default: { appenders: ['defaultLogger'], level: 'error' } }
});
const logger = log4js.getLogger('defaultLogger');
logger.level = 'debug';

const express = require('express');
const path = require('path');
const httplogger = require('morgan');
const bodyParser = require('body-parser');
const config = require("./config.js");
const visualizeWorld =require("./routes/visualizeWorld.js");
const visualizeBMS =require("./routes/visualizeBms.js");
const visualizeSemakau =require("./routes/visualizeSemakau.js");
const visualizeJurong =require("./routes/visualizeJurong.js");
const visualizeOntoEN = require("./routes/visualizeOntoEN.js");
const visualizeOntoChem = require("./routes/visualizeOntoChem.js");
const visualizeOntokin= require("./routes/visualizeOntokin.js");
const showCO2 = require("./routes/showCO2");
const bmsplot= require("./routes/plotBMSCached.js");
const getAttrList =require("./routes/getAttrList");
const getSpecAttr =require("./routes/getSpecificLiteralAttrCached");
const MAUPlot = require("./routes/plotMAU");
const HW =require("./routes/runHeatWasteNetworkMap");
const PPCO2 = require("./routes/powerplantCO2");
const ppMap = require('./routes/mapPowerPlant');
const semakauMap = require("./routes/mapSemakau");
const b2Map = require("./routes/mapB2");
const ppalt = require("./routes/mapPPAlt");

const literalData = require('./agents/GetLiteralData');
const getChildrenSingle = require('./routes/getChildrenSingle');

const BMSWatcher = require('./agents/setBMSWatcher');
let agentWatcher = require('./agents/msgFace');

const app = express();
let port = config.port;
process.env.UV_THREADPOOL_SIZE = 128;

app.set('views', __dirname + '/views');
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
app.use('/b2map', b2Map);
app.use("/hw", HW);
app.use("/mauplot", MAUPlot);
app.use("/getAttrList", getAttrList);
app.use("/getSpecAttr", getSpecAttr);

const httpServer = require('http').Server;
let http = new httpServer(app);
const io = require('socket.io')(http);
const watcherReturn = BMSWatcher();
const ev = watcherReturn.watchEvent;
const bmsWatcher = watcherReturn.bmsWatcher;

agentWatcher.init(io);

//When any change happened to the file system
ev.on('change', function (data) {
    logger.debug("update event: "+" on "+data.uri+"_nodata");
    io.to(path.normalize(data.uri)+"_nodata").emit("update", {uri:data.uri, filename:data.filename});
    io.to(path.normalize(data.uri)+"_data").emit("update", data);
});

/*socket io***/

io.on('connection', function(socket){

    socket.on('join', function (uriSubscribeList) {
        //May be do some authorization

        console.log('client join');
        let sl = JSON.parse(uriSubscribeList);
        logger.debug(sl);
        sl.forEach(function (uri2Sub) {
            let diskLoc = uri2Sub.uri.replace("http://www.theworldavatar.com", config.root)
                .replace("http://www.jparksimulator.com", config.root);


            let affix = uri2Sub.withData? "_data" :"_nodata";
            diskLoc = path.normalize(diskLoc);
            socket.join(diskLoc+affix);
                   console.log(socket.id, "joined", diskLoc+affix);



            //TODO:check client legnth first, if 0 ,first join, ask to register for data


            if(uri2Sub.withData){
                let clients = io.sockets.adapter.rooms[diskLoc+affix].sockets;
                //to get the number of clients
                let numClients = (typeof clients !== 'undefined') ? Object.keys(clients).length : 0;
                logger.debug("number of clients in room: "+numClients);
                if (numClients < 2 ){//first join for data, register for data now
                    logger.debug("first client for this node ,register for data change");
                    bmsWatcher.register(diskLoc,"worldnode", true);
                }

                console.log(diskLoc);
                literalData( function (err, initialData) {
                    //get initial by db access
                    logger.debug("send initial data");
                    socket.emit("initial",initialData);
                }, diskLoc);


            }
        });
        logger.debug("@@@@@@@@@@@@@@@@");
        logger.debug(io.sockets.adapter.rooms)
    });
    socket.on('leave', function (uriSubscribeList) {
        //May be do some authorization
        let sl = JSON.parse(uriSubscribeList);
        sl.forEach(function (uri) {
			        let diskLoc = uri.replace("http://www.theworldavatar.com", config.root)
            .replace("http://www.jparksimulator.com", config.root);
			diskLoc = path.normalize(diskLoc);
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
  let err = new Error('Not Found');
  err.status = 404;
  next(err);
});

// error handler
app.use(function(err, req, res) {
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
