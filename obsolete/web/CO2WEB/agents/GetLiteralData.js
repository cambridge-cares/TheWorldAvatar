/**
 * Get all literal data defined in a file
 * A rewritten getLiteral module that also accmodates bms data(Or any other time series data under this format)
 * bms data will be packed under one name, while value field contains an array
 */



/**
 * Created by Shaocong on 9/13/2017.
 * This module takes all owl data that has an literal
 */
//design a SPARQL query to retrieve all literal data

var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

const util = require('util')
const parser = require('../agents/rdfParser');
const fs = require('graceful-fs'),
    path = require('path'),
    config = require('../config')
/*SPRAQL Query******************/

/**
 * use custom filter literal for now
 * use this query when switched to unified endpoint
 *
 const SPA = `
 SELECT ?literal
 WHERE { ?s ?p  ?literal.
FILTER isLiteral(?literal)}
 `;
 ***/
/***********************************/
//    ?DataPoint rdf:type owl:NamedIndividual;

const SPA = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>

SELECT distinct ?o ?Unit ?eq
WHERE { 
        ?s ?p ?o.
        OPTIONAL{?s system:hasUnitOfMeasure ?unit.}
        OPTIONAL{?s system:hasElectricalRepresentation  ?eq.}

        }
`;



/***
find all literal data contained in an iri entity 
***/
function LiteralData(callback, nodeloc, opts) {

    queryLiteralSingle(nodeloc, function (err, result) {
        if(err){
            logger.debug(err)
            callback(err);
            return;
        }
        let resultsB
        if(!result.eq){//no equivalent entity
            resultsB = packTimeseriesDataIfAny(result.nvpairs);
            if(opts&&opts.specificNames instanceof  Array && opts.specificNames.length >0){
                console.log("names specified")
                console.log(resultsB)
                resultsB = filterVNames(resultsB, opts.specificNames);
                console.log(resultsB)
                console.log(opts.specificNames)
            }
            callback(null, resultsB);
            return;
        }
        queryLiteralSingle(result.eq, function (err,resultEq) {//query attributes defined under equivalent entity
            if(err){
                logger.debug(err)
                callback(null, resultsB);
                return;
            }
            console.log(result.nvpairs)
            console.log(resultEq.nvpairs)

            let results = packTimeseriesDataIfAny(result.nvpairs.concat(resultEq.nvpairs));
            if(opts&&opts.specificNames instanceof  Array && opts.specificNames.length >0){
                console.log("names specified")
                results = filterVNames(results, opts.specificNames);
            }
            callback(null,results )
        })
    });



/***
utility function:
find in the result list item with specific names

returns list of found items
**/
    function filterVNames(results, namelist){

       return namelist.map((name)=>{

           let found = results.filter((datum)=>{
              return datum.name === name;
          });
            return found.length>0?found[0]:null;

       });

    }


/***
pack timeseries data into an array if it exists in an data array
input: all data before processing
return packed data
***/
    function packTimeseriesDataIfAny(allData) {
        logger.debug("literal data all before parsing timeseries data")
        let timeSeriesData = {}, normalData = []

        allData.forEach((datum)=>{
            if(datum.name.includes('@')){

                let seriesName = datum.name.split('@')[0];
                timeSeriesData[seriesName] = timeSeriesData[seriesName]?timeSeriesData[seriesName]:{
                    name: seriesName,
                    value:[],
                    datatype: datum.datatype,
                    p:datum.p,
                    unit:datum.unit,
                    seriestype: true
                };//init this series if no exists

                let timeStr = datum.name.split('@')[1];
                let time = timeParser(timeStr);

                timeSeriesData[seriesName].value.push({value: datum.value, time:time});

            } else{
                normalData.push(datum);
            }
        })
        if(Object.keys(timeSeriesData).length < 1){//no time series data
            return allData;//return inital
        } else {
            //sort all value field
            timeSeriesDataVs = Object.values(timeSeriesData);

            timeSeriesDataVs.forEach((data) =>{
                data.value = data.value.sort(compareTime);
            })

            logger.debug("get literal data after parsing timeseries data");
            logger.debug(timeSeriesDataVs.concat(normalData));
            return timeSeriesDataVs.concat(normalData);
        }
        //pack timeSeriesData
    }

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

/***
find all attributed and equivalent iris defined under one iri entity
input:
nodeloc   entity iri
callback
@returns {eq iri, name-value pairs of attributes}

***/
    function queryLiteralSingle(nodeloc, callback) {
        nodeloc = nodeloc.replace("http://www.theworldavatar.com", config.root).replace("http://www.jparksimulator.com", config.root);
        console.log(nodeloc);
        fs.readFile(nodeloc, function (err, file) {
            if(err){
                logger.debug(err)
                callback(err);
                return;
            }

            try{
                logger.debug("start parsing")
                var mparser = new parser.RdfParser({uri: nodeloc, file :file});
                logger.debug("end parsing")

                mparser.mquery( SPA, function (err, data) {//each data point
                    logger.debug("query")

                    if(err){
                        logger.debug(err);
                        callback(err);
                        return
                    }

                    let eq = getEQ(data);
                    console.log("uri: "+ eq)
                    let nvpairs = filterNconstruct(data)

                    callback(null, {eq, nvpairs});


                });
            }catch(err){
                throw err
            }

        });

    }



  /**
  unpack equivalent iri
  **/
    function getEQ(spos) {
        let found =  spos.find(function (spo) {
            return spo['?eq'];
        });
        if(found&&found['?eq']['value']){
            let str = found['?eq']['value'].trim();
            let strArr = str.split('#');
            return strArr[0];
        }
        return null;
    }

    function filterNconstruct(spos){
        //  logger.debug(spos)
        return spos.filter(function (spo) {
            //logger.debug(spo)
            return spo['?o']['termType'] === "Literal";
        }).map(function (spl) {
            let name = getVNameFromUri(spl['?s']['value']);
            let p = spl['?p']['value'];
            let value = spl['?o']['value'];
            let datatype = getVNameFromUri(spl['?o']['datatype']['value']);
            //console.log(spl['?o']['datatype']['value'])
            let unit =spl['?unit']? getVNameFromUri(spl['?unit']['value']):"";

            if(name&&value&&p){
                return {name, value, p, datatype, unit};
            }
        }).filter((item)=>{return item!==undefined&&item!==null});
    }



    function getVNameFromUri(uri){
        let arr  = uri.split('#');
        if(arr.length < 2){
            return "";
        }
        return arr[arr.length-1];
    }
}


module.exports = LiteralData;