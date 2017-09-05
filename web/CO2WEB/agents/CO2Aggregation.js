/**
 * Created by Shaocong on 7/19/2017.
 */
const $rdf = require('rdflib');
const config = require('../config')
const util = require('util')
const parser = require('../util/rdfParserObsolete')({fileUrl:config.landLotNode});

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

    var CO2Addresult;
function getCO2Aggregation(callback) {
//TODO: error handle


   if(!CO2Addresult){
    CO2Addresult = 0;
    parser.graph.query($rdf.SPARQLToQuery(SPA, false, parser.graph), function (data) {//each data point

        //      console.log("!!!!!!!!!!!!!!!!!!!!")
        //console.log(arguments[i])
       // console.log( data['?ValueOf_CarbonEmissions']['value']);
        // console.log(counter++)

        CO2Addresult+=parseFloat(data['?ValueOf_CarbonEmissions']['value']);

    }, null, function (err) {//when all is done
       // console.log("@@@@@@@@@@@done")
       // console.log(result)
       if(err){
		   console.log(err);
	callback(err);       
}
        //Now result is ready
        callback(null, CO2Addresult);
    });
  } else{
       console.log(CO2Addresult);
        callback(null, CO2Addresult);
}






}


module.exports = getCO2Aggregation;