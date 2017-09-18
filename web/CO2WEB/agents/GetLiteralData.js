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
SELECT distinct ?o
WHERE { ?s ?p  ?o. }
`;

function LiteralData(nodeloc, callback) {
//TODO: error handle
                                                           //todo: delete this poweplant subroute
    nodeloc = nodeloc.replace("http://www.theworldavatar.com", path.join(config.root,"powerplants")).replace("http://www.jparksimulator.com", path.join(config.root,"powerplants"));
    console.log(nodeloc)
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
                }

                let nvpairs = filterNconstruct(data)
                logger.debug("Filtering for literals only:")
                logger.debug(nvpairs)
                callback(null, nvpairs)
            });
        }catch(err){
            throw err
        }

    });

    function filterNconstruct(spos){
        return spos.filter(function (spo) {
            //logger.debug(spo)
            return spo['?o']['termType'] === "Literal";
        }).map(function (spl) {
            let name = getVNameFromUri(spl['?s']['value']);
            let value = spl['?o']['value'];
            if(name&&value){
                return {name, value};
            }
        }).filter((item)=>{return item!==undefined&&item!==null});
    }



    function getVNameFromUri(uri){
        let arr  = uri.split('#');
        if(arr.length < 2){
            return null;
        }
        return arr[arr.length-1];
    }
}


module.exports = LiteralData;