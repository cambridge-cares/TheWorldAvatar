/**
 * Created by Shaocong on 10/17/2017.
 */
//convert google GPS coordi to 1984w coordi(the one used in our own)

    var proj4 = require('proj4')
var convertCoordinate = function (GPSLong, GPSLat, google2Owl) {
//https://github.com/proj4js/proj4js
    var googleProjection = 'EPSG:4326'; //google
    var ourProjection = 'EPSG:3857';//our
//logger.debug("convert coordis: ["+parseInt(GPSLong)+", "+parseInt(GPSLat)+"] to "+proj4(fromProjection, toProjection, [parseInt(GPSLong),parseInt(GPSLat)]));

    return google2Owl?converted(googleProjection, ourProjection) : converted(ourProjection, googleProjection);
    function converted(fromProjection, toProjection){

        GPSLong  = typeof GPSLong === "string"?parseFloat(GPSLong) : GPSLong
        GPSLat  = typeof GPSLat === "string"?parseFloat(GPSLat) : GPSLat

        var result =  proj4(fromProjection, toProjection, [GPSLong,GPSLat]);

        return {x: result[0], y:result[1]};
    }

};
//1.258856, 103.678836



console.log(convertCoordinate(	103.678836,		1.258856, true ))
console.log(convertCoordinate(	11552746.986,	134561.583, false ))
