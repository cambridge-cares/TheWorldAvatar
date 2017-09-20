/**
 * Created by Shaocong on 9/20/2017.
 */
//first get All child, then request&query each to get coordinates

const xmlProcessor = require("./fileConnection"),
config = require("../config")


xmlProcessor.readConnections({topnode : config.semakauNode}, function (err, results) {

    if(err){
        console.log(err);
        return;
    }

    console.log(results.geoCoords)



});