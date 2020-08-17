/**
A module that reads all coordinates from all powerplant files.
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

const xmlParser = require('./fileConnection2Way'),
      rdfParser = require('./rdfParser'),
      config = require('../config'),
      async = require('async'),
      path = require('path'),
      fs = require('graceful-fs'),
      worldNode  = config.ppNode;
      processor = Object.create(xmlParser);


function readPPCoordi(callback) {
    //readPPChildren
    //request on each to get geo info
    processor.init({});

    processor.doConnect(worldNode,0).then((PPchildren)=>{


            PPchildren = PPchildren.map((item)=>item['target'])
			console.log("ppcildren= "+PPchildren);
            //now, read each file, parse as rdf, query its geographic information
            let listUrinLoc = xmlParser.uriList2DiskLoc(PPchildren,config.root);


            async.concat(listUrinLoc, queryCoord , function (err, dataset) {
            if(err){
				console.log(err);
               // callback(err);
                return;
            }
                //logger.debug(JSON.stringify(dataset))
                //construct dataset to google coordi format
                let datasetF = []
                dataset.forEach(item=>{ if(item){datasetF.push(item)}})


                let formatted = datasetF.map(function (item) {
                    for(let uri in item){
                        if(item.hasOwnProperty(uri)){
                            //let toGoogle = xmlParser.convertCoordinate(item[uri].x, item[uri].y, false);
                            return {uri: uri, location :{lat: parseFloat(item[uri].y), lng:parseFloat(item[uri].x)}}
							//return {uri: uri, location :{lat: parseFloat(item[uri].x), lng:parseFloat(item[uri].y)}}
                        }
                    }
                })



                callback(null, formatted);
            });




    });




    function queryCoord(fileInfo, callback){

        getChildFile(fileInfo.diskLoc, function (err, file) {
            if(err || !file){
                callback(null,null);//don't throw err
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
/**
readPPCoordi((err, result)=>{
    "use strict";
    if(err) {console.log(err)};
    console.log('print result')
    console.log(result)
})
 ***/
module.exports = readPPCoordi;
