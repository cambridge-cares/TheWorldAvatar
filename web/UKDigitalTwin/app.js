/***
 * app main
 * @type {*}
 */
 
var config = require("./config.js");
var path = require('path');

var log4js = require('log4js'); // log4js is a Java-based logging utility or framwork
log4js.configure({
  appenders: { defaultLogger: { type: 'file', filename: path.join(config.logDir, 'js-server.log') } },
  categories: { default: { appenders: ['defaultLogger'], level: 'error' } }
});
var logger = log4js.getLogger('defaultLogger'); // 'defaultLogger' is a loggerCategory
logger.level = 'debug';

var express = require('express');
var httplogger = require('morgan');
var request =require("request");
var bodyParser = require('body-parser');
var util = require('util');
var UKontoTwinMap = require('./routes/ontoTwinUK');
// var UKontoTwinMapAttr = require('./routes/powerPlantAttr');

var app = express(); // this object app has methods 
                     //for routing HTTP requests, configuring middleware, rendering HTML views, 
                    //registering a template engine, and modifying application settings that control how the application behaves.

// Enable cross-domain requests
const cors = require('cors');
app.use(cors());			
					
var port = config.port;
process.env.UV_THREADPOOL_SIZE = 128;

app.set('view engine', 'pug');
app.use(httplogger('dev'));
app.use((req, res, next) => {
    logger.debug("res.header");

  res.header('Access-Control-Allow-Origin', '*');
  next();
});
function setHeader(res, mpath){
  logger.debug("path"+ mpath);
  res.setHeader("Content-Type","text/xml");
  res.setHeader('Access-Control-Allow-Origin', '*');

  res.setHeader("Content-Disposition","inline");
    logger.debug("SEtting headers");
}
/*body parser*/
app.use(bodyParser.text({ type: 'application/json' }));

/*serve static file***/
app.use(express.static(path.join(__dirname, 'public'))); // declare the application of middleware
app.use(express.static(path.join(__dirname, 'ROOT'), {'setHeaders': setHeader}));

/*posting to dataObserve to get orginal data & register for future data change*/
// app.use("/getUKPPAttrList", getUKPPAttrList);
app.use('/ontoTwinUK', UKontoTwinMap);

// test for a new router based on the agent named getPPAttr.js
// app.use('/powerplant/:url', UKontoTwinMapAttr)

// app.use('/ppmap', ppMap);

var http = require('http').createServer(app);
var io = require('socket.io')(http);
io.set('transports', ['websocket','polling']);


/*future data change will be post to this route*/

//When any change happened to the file system
let testId = null

/*socket io***/
io.on('connection', function(socket){
socket.on('join', function (uriSubscribeList) {
    //May be do some authorization
   testId = socket.id
    console.log('client join');
    let sl = JSON.parse(uriSubscribeList);
    //logger.debug(sl)
    if('endpoint' in sl){
        console.log('join event: to end points')
        let epUrl= sl['url']; let uriList = sl['subscribeList'];
        socket.join(epUrl+'_endpoint');//join the room
        epInformer.registerSubsriber( epUrl, uriList, qstr, socket.username);
        return;
    }
    
    sl.forEach(function (uri2Sub) {
        let diskLoc = uri2Sub.uri.replace("http://www.theworldavatar.com", config.root)
            .replace("http://www.jparksimulator.com", config.root);


        let affix = uri2Sub.withData? "_data" :"_nodata";
        diskLoc = path.normalize(diskLoc)
        socket.join(diskLoc+affix);
        let rooms = Object.keys(io.sockets.adapter.rooms)
        //console.log(rooms)
             // console.log(socket.id, "joined", diskLoc+affix);



        //TODO:check client legnth first, if 0 ,first join, ask to register for data


        if(uri2Sub.withData){
            var clients = io.sockets.adapter.rooms[diskLoc+affix].sockets;
            
//to get the number of clients
            var numClients = (typeof clients !== 'undefined') ? Object.keys(clients).length : 0;
            logger.debug("number of clients in room: "+numClients);
            if (numClients < 2 ){//first join for data, register for data now
                console.log("first client for this node ,register for data change")
                bmsWatcher.register(diskLoc,"worldnode", true);
            }


                literalData( function (err, initialData) {
                    //get initial by db access
                    logger.debug("send initial data");
                    socket.emit("initial",initialData);
                }, diskLoc);


        }
    })


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

module.exports = app;
