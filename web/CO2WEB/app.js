/***
 * app main
 * @type {*}
 */
var log4js = require('log4js'); // log4js is a Java-based logging utility or framwork
log4js.configure({
  appenders: { defaultLogger: { type: 'file', filename: 'C:/jps/temp/logs/js-server.log' } },
  categories: { default: { appenders: ['defaultLogger'], level: 'error' } }
});
var logger = log4js.getLogger('defaultLogger'); // 'defaultLogger' is a loggerCategory
logger.level = 'debug';

var express = require('express');
var path = require('path');
var httplogger = require('morgan');
var request =require("request");
var bodyParser = require('body-parser');
var util = require('util');
var config = require("./config.js");
var getAttrList =require("./routes/getAttrList");

if (config.onCares){ //configure to be on Claudius or on CMCL server
    var visualizeWorld =require("./routes/visualizeWorld.js");
    var visualizeBMS =require("./routes/visualizeBms.js");
    var visualizeSemakau =require("./routes/visualizeSemakau.js");
    var visualizeJurong =require("./routes/visualizeJurong.js");
    var visualizeOntoEN = require("./routes/visualizeOntoEN.js");
    var visualizeOntoChem = require("./routes/visualizeOntoChem.js");
    var visualizeAgent = require("./routes/visualizeAgent.js");
    var visualizeOntokin= require("./routes/visualizeOntokin.js");
    var visualizeOntoEN = require("./routes/visualizeOntoEN.js");

    var visualizeOntokinR= require("./routes/visualizeOntokinRemote.js");

    var getAttrList =require("./routes/getAttrList");
    var getSpecAttr =require("./routes/getSpecificLiteralAttrCached");

    var showCO2 = require("./routes/showCO2");
    var bmsplot= require("./routes/plotBMSCached.js");


    var MAUPlot = require("./routes/plotMAU")
    var MAU = require("./routes/runMAU");
    var HW =require("./routes/runHeatWasteNetworkMap")
    // var PPCO2 = require("./routes/powerplantCO2Cached");
    var PPCO2 = require("./routes/powerplantCO2");

    var ppMap = require('./routes/mapPowerPlant');

    var semakauMap = require("./routes/mapSemakau")
    var b2Map = require("./routes/mapB2");
    var ppalt = require("./routes/mapPPAlt");
    var parallelWorld = require('./routes/parallelWorld');
    var wteMap= require('./routes/wTEroute');

    var admsEpi= require('./routes/admsEpi');

    var essMap = require('./routes/ess');
    var DESPlot = require('./routes/DESPlot');
    var literalData = require('./agents/GetLiteralData');
    var getChildrenSingle = require('./routes/GetChildrenSingle');
    var agentWatcher = require('./agents/msgFace');
}
var ppMap = require('./routes/mapPowerPlant');
var visualizeJurong =require("./routes/visualizeJurong.js");
var visualizeWorld =require("./routes/visualizeWorld.js");
var ontoTwinMap= require('./routes/ontoTwinUK');
var BMSWatcher = require('./agents/setBMSWatcher');
let setEpWatcher = require('./agents/setEPWatcher');


var app = express();
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
app.use(express.static(path.join(__dirname, 'public')));
app.use(express.static(path.join(__dirname, 'ROOT'), {'setHeaders': setHeader}));
if (config.onCares){
    app.use('/getChildrenSingle',getChildrenSingle);
    app.use('/visualizeAgent', visualizeAgent);
    app.use('/visualizeWorld', visualizeWorld);
    app.use('/visualizeBMS', visualizeBMS);
    app.use('/visualizeSemakau', visualizeSemakau);
     

    app.use('/PowerPlantCO2',  PPCO2);
    app.use('/semakaumap', semakauMap);
    app.use('/ppalt', ppalt);
    app.use('/pwScenario', parallelWorld);
    app.use('/essMap', essMap);
    app.use('/wteMap', wteMap);

    app.use('/JPS_SHIP', admsEpi);

    app.use('/JurongIsland.owl/showCO2', showCO2);
    app.use('/visualizeOntoEN',visualizeOntoEN);
    app.use('/visualizeOntoChem',visualizeOntoChem);
    app.use('/visualizeOntokin',visualizeOntokin);

    app.use("/bmsplot", bmsplot);

    app.use('/ppmap', ppMap);
    app.use("/DESplot", DESPlot);

    app.use('/b2map', b2Map)
     app.use("/hw", HW);

    app.use("/DESplot", DESPlot);
    app.use("/mauplot", MAUPlot);
    app.use("/MAU", MAU);
    app.use("/getAttrList", getAttrList);
    app.use("/getSpecAttr", getSpecAttr);


    app.use('/visualizeOntokinRemote',visualizeOntokinR);
}

/*posting to dataObserve to get orginal data & register for future data change*/
app.use("/getAttrList", getAttrList);

app.use('/ontoTwinUK', ontoTwinMap);
app.use('/visualizeJurong', visualizeJurong);
app.use('/visualizeWorld', visualizeWorld);
app.use('/ppmap', ppMap);

var http = require('http').createServer(app);
var io = require('socket.io')(http);
io.set('transports', ['websocket','polling']);


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
	ev.on('update', function (data) {
    logger.debug("update event: "+" on "+data.uri+"_nodata");
	    //let rooms = io.sockets.adapter.rooms;
   //logger.debug(rooms[path.normalize(data.uri)].sockets);
    //update direct clients
	if(!('data' in data) || data.data ===null){
		console.log('data update for: '+data.uri)
    io.in(path.normalize(data.uri)+"_nodata").emit("update", {uri:data.uri, filename:data.filename});
    } else {
		//console.log('update event:'+path.normalize(data.uri)+"_data")
		//console.log('now update');
		//console.log('rooms')
		let testid =path.normalize(data.uri)+"_data"
		let rooms = Object.keys(io.sockets.adapter.rooms)
				//console.log(rooms)
		io.in(path.normalize(data.uri)+"_data").emit("update", data);
		    io.in(path.normalize(data.uri)+"_nodata").emit("update", {uri:data.uri, filename:data.filename});
}
})

if (config.onCares){
agentWatcher.init(io);
	ev.on('update', function (data) {
    logger.debug("update event: "+" on "+data.uri+"_nodata");
	    //let rooms = io.sockets.adapter.rooms;
   //logger.debug(rooms[path.normalize(data.uri)].sockets);
    //update direct clients
	if(!('data' in data) || data.data ===null){
		console.log('data update for: '+data.uri)
    io.in(path.normalize(data.uri)+"_nodata").emit("update", {uri:data.uri, filename:data.filename});
    } else {
		//console.log('update event:'+path.normalize(data.uri)+"_data")
		//console.log('now update');
		//console.log('rooms')
		let testid =path.normalize(data.uri)+"_data"
		let rooms = Object.keys(io.sockets.adapter.rooms)
				//console.log(rooms)
		io.in(path.normalize(data.uri)+"_data").emit("update", data);
		    io.in(path.normalize(data.uri)+"_nodata").emit("update", {uri:data.uri, filename:data.filename});
}
})
}
//When any change happened to the file system
let testId = null


const aepWatcher = setEpWatcher();
const epChangeEv = aepWatcher.watchEvent;
const epInformer = aepWatcher.epChangeEmitter;
epChangeEv.on('new', function (data) {
    //console.log('new endpoint modfication event');
    //todo: logic of subscription
    io.to(path.normalize(data.endpoint)+"_endpoint").emit("new", data.data);
    
})

let qstr = `
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction_mechanism: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
SELECT distinct  ?parent ?uri
WHERE {

	{
    ?parent rdf:type reaction_mechanism:ChemicalReaction .
    ?parent reaction_mechanism:hasProduct ?product .
    ?product owl:sameAs ?uri .
	}
	
	UNION{
    ?phase ontokin:containedIn ?parent .
    ?a ontokin:belongsToPhase ?phase .
    ?a rdf:type reaction_mechanism:ChemicalReaction .
	}
	UNION{
    ?parent rdf:type reaction_mechanism:ChemicalReaction .
    ?parent reaction_mechanism:hasReactant ?reactant .
    ?reactant owl:sameAs  ?uri .
	}

		UNION
	{
	    ?species owl:sameAs  ?parent .
		?species ontokin:hasThermoModel ?thermoModel.
		    ?thermoModel ontokin:hasQuantumCalculation ?uri .

	}
	
} `

//let epUrl = 'http://theworldavatar.com/rdf4j-server/repositories/ontokin'
//let resourceId= 'http://www.theworldavatar.com/kb/ontokin/diesel-surrogate-detailed.owl#ChemicalReaction_186504352510197_1097'
//let rid2 ='http://www.theworldavatar.com/kb/ontokin/diesel-surrogate-detailed.owl#Species_186505767336106_106'
//ep.registerSubsriber(epUrl,[resourceId, rid2],qstr,'whatever')

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
