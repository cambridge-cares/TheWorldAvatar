/**
Get all coordinates defined on Semakau,
 This module should be rewritten in future to unite with others, but currently different plant owl files have different way of defining coordinates and has made this task impossible.
 */
//first get All child, then request&query each to get coordinates
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';
const xmlProcessor = require("./fileConnection"),
config = require("../config")


/***
 * A function to get semakau coordinates information
 * @param callback
 */
function  getSemakauCoordi(callback) {
    /***
     *
     */
    xmlProcessor.getChildrenRecur({topnode : config.semakauNode}, function (err, results) {

        if(err){
            console.log(err);
            callback(err)
            return;
        }


        logger.debug("!!!!!!!!!!!!!!!!!!!!!!!")
        logger.debug(JSON.stringify(results.geoCoords))

        callback(null, format(results.geoCoords));


    });

    function format(list) {
        return list.map((item)=>{return {uri: item.url, location:{lat: item.coord.y, lng:item.coord.x}, type:  item.type}});
    }

}



module.exports = getSemakauCoordi
