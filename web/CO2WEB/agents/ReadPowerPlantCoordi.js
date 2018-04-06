/**
A module that reads all coordinates from all powerplant files.
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

const xmlParser = require('./fileConnection'),
      rdfParser = require('./rdfParser'),
      config = require('../config'),
      async = require('async'),
      path = require('path'),
      fs = require('graceful-fs'),
      worldNode  = config.worldNode;

function readPPCoordi(callback) {
    //readPPChildren
    //request on each to get geo info

    fs.readFile(worldNode, function (err, file) {
        if(err){
            callback(err)
            return;
        };
        try{
            let root = xmlParser.parseXMLFile(file);
            let PPchildren = xmlParser.getPPChildren(root);
            //now, read each file, parse as rdf, query its geographic information
            let listUrinLoc = xmlParser.uriList2DiskLoc(PPchildren,config.ppFolder);


            async.concat(listUrinLoc, queryCoord , function (err, dataset) {
            if(err){
				console.log(err);
               // callback(err);
                return;
            }
                //logger.debug(JSON.stringify(dataset))
                //construct dataset to google coordi format
                let formatted = [];

                formatted = dataset.map(function (item) {
                    for(let uri in item){
                        if(item.hasOwnProperty(uri)){
                            //let toGoogle = xmlParser.convertCoordinate(item[uri].x, item[uri].y, false);
                            return {uri: uri, location :{lat: parseFloat(item[uri].x), lng:parseFloat(item[uri].y)}}
                        }
                    }
                })




                callback(null, formatted);
            });


        }catch(err){
            callback(err)
        }

    });




    function queryCoord(fileInfo, callback){

        getChildFile(fileInfo.diskLoc, function (err, file) {
            if(err || !file){
                callback(err);
                return;
            }

            var mparser = new rdfParser.RdfParser({file: file.toString(), uri:fileInfo.uri});

            mparser.geoCoordsQuery(callback);
        })

    }
    function getChildFile(loc, callback){
        fs.readFile(loc, callback);
    }

}


module.exports = readPPCoordi;








