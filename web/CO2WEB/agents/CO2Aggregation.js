/**
 * Created by Shaocong on 7/19/2017.
 */
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
    ?x system:numericalValue ?ValueOf_CarbonEmissions.}

order by DESC(?ValueOf_CarbonEmissions)
`;
/***********************************/

function getCO2Aggregation(callback) {
//TODO: error handle

    fs.readFile(config.landLotNode, function (err, file) {
		if(err || !file){
			console.log(err);
			callback(err);
		}
        var mparser = new parser.RdfParser({uri: config.landLotNode, file :file});

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
            callback(null, CO2Addresult);
        });
    });
}


module.exports = getCO2Aggregation;