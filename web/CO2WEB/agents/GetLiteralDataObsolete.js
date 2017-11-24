/**
  This module is obsoleted and has been replaced by the new GetLiteral that also accomodates bms data.

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
 * TODO:RDFLIB MODULE does not support isLiteral,
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
    PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>

SELECT distinct ?o ?Unit ?eq
WHERE { 
        ?s ?p ?o.
        OPTIONAL{?s system:hasUnitOfMeasure ?unit.}
        OPTIONAL{?s system:hasElectricalRepresentation  ?eq.}

        }
`;


function LiteralData(nodeloc, callback) {
//TODO: error handle

    queryLiteralSingle(nodeloc, function (err, result) {
        if(err){
            logger.debug(err)
            callback(err);
            return;
        }
        if(!result.eq){
             callback(null, result.nvpairs);
             return;
        }
        queryLiteralSingle(result.eq, function (err,resultEq) {
            if(err){
                logger.debug(err)
                callback(err);
                return;
            }
            console.log(result.nvpairs)
            console.log(resultEq.nvpairs)

            callback(null, result.nvpairs.concat(resultEq.nvpairs))
        })
    });



    function queryLiteralSingle(nodeloc, callback) {
        //todo: delete this poweplant subroute
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