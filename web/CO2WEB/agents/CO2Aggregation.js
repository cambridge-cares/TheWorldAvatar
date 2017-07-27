/**
 * Created by Shaocong on 7/19/2017.
 */
const $rdf = require('rdflib');
const config = require('../config')
const util = require('util')
const parser = require('../util/rdfParser')({fileUrl:config.landLotNode});

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

    let result;
function getCO2Aggregation(callback) {
//TODO: error handle


   if(!result){
    result = 0;
    parser.graph.query($rdf.SPARQLToQuery(SPA, false, parser.graph), function (data) {//each data point

        //      console.log("!!!!!!!!!!!!!!!!!!!!")
        //console.log(arguments[i])
       // console.log( data['?ValueOf_CarbonEmissions']['value']);
        // console.log(counter++)

        result+=parseFloat(data['?ValueOf_CarbonEmissions']['value']);

    }, null, function (err) {//when all is done
       // console.log("@@@@@@@@@@@done")
       // console.log(result)
       if(err){
	callback(err);       
}
        //Now result is ready
        callback(null, result);
    });
  } else{
        callback(null, result);
}






}


module.exports = getCO2Aggregation;