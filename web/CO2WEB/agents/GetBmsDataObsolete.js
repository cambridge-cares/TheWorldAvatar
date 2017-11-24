/**
 * This module is obsoleted, and has been unified with GetLiteralData module.
 *
 * specifically require data from bms
 * In future it will be done through http request when bms is hosted sepaly
 * but now we do disk loc search
 */
//design a SPARQL query to retrieve all data point

var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

const util = require('util')
const parser = require('../agents/rdfParser');
const fs = require('graceful-fs')
/*SPRAQL Query******************/
const SPA = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>

    select distinct ?DataPoint ?ValueOf_DataPoint
    where {
    ?DataPoint system:hasValue ?x.
    ?x system:numericalValue ?ValueOf_DataPoint.
    OPTIONAL{?x system:hasUnitOfMeasure ?Unit.}
    }
`;
/***********************************/
//    ?DataPoint rdf:type owl:NamedIndividual;


function BMSData(nodeloc, callback) {

//TODO: error handle
   fs.readFile(nodeloc, function (err, file) {
      if(err){
          logger.debug(err)
          callback(err);
          return;
      }
       var mparser = new parser.RdfParser({uri: nodeloc, file :file});

       //order by DESC(?ValueOf_CarbonEmissions)

       mparser.mquery( SPA, function (err, data) {//each data point
           if(err){
               logger.debug(err);
               callback(err);
           }
           logger.debug("raw query data:")
           logger.debug(data)
           //parseFloat(item['?ValueOf_DataPoint']['value']);
           //parseFloat(item['?DataPoint']['value']);

           let unitUri = data[0]&&data[0]['?Unit']?data[0]['?Unit']['value']:"";

           let unitUriArr = unitUri.split('#');
           let unit = unitUriArr[unitUriArr.length - 1];
           unit =unit?unit:"";
         logger.debug(unit)
           var resultArr = data.map(function (item) {
               let nameStr = item['?x']['value'];
               let timeStr = nameStr.split('@')[1];
               let time = timeParser(timeStr);
               return {time: time, value: parseFloat(item['?ValueOf_DataPoint']['value'])};
           });
           resultArr = resultArr.sort(compareTime);
           logger.debug(resultArr)
           callback(null, {data: resultArr, unit:unit})
       });
  });



    //09-06-17-19:10:04
    function timeParser(timeStr) {
var timeParts = timeStr.match(/(\d{2})-(\d{2})-(\d{2})-(\d{2}):(\d{2}):(\d{2})/);
        if(timeParts.length !==7 ){
        throw(new Error("Wrong time format!"));
    }
    var date = new Date("20"+timeParts[3], parseInt(timeParts[1])-1, timeParts[2], timeParts[4],timeParts[5],timeParts[6]);
        return date.getTime();
    }
    var compareTime = function(ta, tb) {
        return ta.time-tb.time;
    }

}

module.exports = BMSData;