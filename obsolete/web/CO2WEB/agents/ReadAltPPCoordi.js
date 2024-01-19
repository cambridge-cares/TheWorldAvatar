/**
 * For Alvind's power plants display, ripped off directly from readPowerPlantCoordi
 * Most likely should be unified into one module in the future.
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
    url = require('url'),
    URL = url.URL,
    altfolder  = new URL(  "/ppAl", config.baseUri),
    faketop = altfolder+"/FakeParent.owl",
    processor = Object.create(xmlParser);


function readAltPPCoordi(callback) {
    //readPPChildren
    //request on each to get geo info
    logger.debug("faketop "+faketop)
    processor.init({});
    processor.doConnect(faketop,0).then((PPchildren)=>{


            logger.debug("alt pp children:")
            logger.debug(PPchildren)
        PPchildren = PPchildren.map((item)=>item['target'])
    
        //now, read each file, parse as rdf, query its geographic information
            let listUrinLoc = xmlParser.uriList2DiskLoc(PPchildren,config.root);

           // logger.debug("!!!!!!!!!!!!!!!!!!!!!!!!!!")
          //  logger.debug(listUrinLoc)

            async.concat(listUrinLoc, queryCoord , function (err, dataset) {
                if(err){
                    callback(err);
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


module.exports = readAltPPCoordi;
