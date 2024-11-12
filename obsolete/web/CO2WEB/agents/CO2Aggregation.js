/**
 * Aggregates CO2 emission extracted from landlort owl files[commented]
 */
const $rdf = require('rdflib');
const config = require('../config')
const util = require('util')
const parser = require('../agents/rdfParser');
const fs = require('graceful-fs')
/*SPRAQL Query******************/
const SPA = `
PREFIX JParkLandLots: <http://www.jparksimulator.com/kb/sgp/jurongisland/JParkLandLots.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>

    select distinct ?LandLot_CarbonEmissions ?ValueOf_CarbonEmissions
    where {
    ?LandLot_CarbonEmissions rdf:type JParkLandLots:CarbonEmissions;
system:hasValue ?x.
    ?x system:numericalValue ?ValueOf_CarbonEmissions.}

order by DESC(?ValueOf_CarbonEmissions)
`;
/***********************************/

//return aggregation of a single query
function getCO2Aggregation(callback) {

try{
    console.log("inside co2agge")
	    var file = fs.readFileSync(config.landLotNode);
	
        var mparser = new parser.RdfParser({uri: config.landLotNode, file :file});

        mparser.mquery( SPA, function (err, data) {//each data point


            if(err){
                console.log(err);
                callback(err);
            }
            var CO2Addresult = 0;
            data.forEach(function(item){
                CO2Addresult+=		parseFloat(item['?ValueOf_CarbonEmissions']['value']);
            })

            CO2Addresult = parseFloat(CO2Addresult)/(365*24*60*60);

            CO2Addresult = CO2Addresult.toFixed(4);//format result into float like 1.00
            callback(null, CO2Addresult);
        });
}catch(err){
	
			console.log(err);
			callback(err);
		
}
 
    };



module.exports = getCO2Aggregation;