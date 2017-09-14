/**
 * Created by Shaocong on 7/19/2017.
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

const $rdf = require('rdflib');
const config = require('../config')
const util = require('util')
const parser = require('../agents/rdfParser');
const fs = require('fs')
/*SPRAQL Query******************/
const SPA = `
PREFIX JParkLandLots: <http://www.jparksimulator.com/JParkLandLots#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>

    select distinct ?LandLot_CarbonEmissions ?ValueOf_CarbonEmissions
    where {
    ?LandLot_CarbonEmissions rdf:type JParkLandLots:CarbonEmissions;
    system:hasValue ?x.
    ?x system:numericalValue ?ValueOf_CarbonEmissions.
    }
`;
/***********************************/

let co2AggResult;
function getCO2Aggregation(callback) {
//TODO: error handle

    var file = fs.readFileSync(config.landLotNode)

    var mparser = new parser.RdfParser({uri: config.landLotNode, file :file});

    //order by DESC(?ValueOf_CarbonEmissions)

    mparser.mquery( SPA, function (err, data) {//each data point

        // console.log("@@@@@@@@@@@done")
        // console.log(result)
        if(err){
            console.log(err);
            callback(err);
        }
        var CO2Addresult = 0;
        data.forEach(function(item){
            CO2Addresult+=		parseFloat(item['?ValueOf_CarbonEmissions']['value']);
        })
        //Now result is ready
      //  co2AggResult = CO2Addresult;
        callback(null, CO2Addresult);
    });

}



module.exports = getCO2Aggregation;