/**
 * Construct a list of all sensors
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

const util = require('util')
const fs = require('graceful-fs')
const xmlParser = require("./fileConnection2Way");
const config = require('../config');
const processor = Object.create(xmlParser);


const bmsNode = config.bmsNode

/*SPRAQL Query******************/
const SPA = `
    PREFIX industrialPark: <http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX xmls: <http://www.w3.org/2001/XMLSchema#>

    select distinct  ?child
    where {
    ?x rdf:type owl:NamedIndividual;
    ?x industrialPark:hasIRI ?child.    
    }
`;
/***********************************/
//    ?DataPoint rdf:type owl:NamedIndividual;

function getAllSensors(callback) {
    //readPPChildren
    //request on each to get geo info
    
    
    processor.process({topnode:bmsNode, unpack:true}).then((children)=>{

            console.log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
             console.log(children);

            children = children.map((conn)=>{return conn.target});
            let sschildren = children.filter((uri)=>{return uri.toLowerCase().includes("sensor")});

            logger.debug("************");
            // OUT repeat

            let uniqss = Array.from(new Set(sschildren));

            uniqss= uniqss.map((uri)=>{
               return uri.replace("/BMS/BMS", '/BMS')
            })
            callback(null, uniqss)
        })







}

module.exports = getAllSensors;