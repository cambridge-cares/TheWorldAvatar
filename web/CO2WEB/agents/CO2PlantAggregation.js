/**
 * Created by Shaocong on 8/31/2017.
 */

var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

const config =  require('../config');
const worldNode = config.worldNode;
const xmlParser = require("./fileConnection");
const parser = require('./rdfParser');
const async = require('async');
const fs =require('graceful-fs')
const util = require('util');
const path = require('path')

var qsTypeCountry = `
    PREFIX system_realization: <http://www.theworldavatar.com/OntoEIP/system_aspects/system_realization.owl#>
        PREFIX system_v1: <http://www.theworldavatar.com/OntoEIP/upper_level/system_v1.owl#>
     PREFIX j.0: <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/technical_system.owl#>
     PREFIX p14: <http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#>
          PREFIX p9: <http://www.theworldavatar.com/OntoEIP/supporting_concepts/space_and_time_v1.owl#>

        select distinct ?Power_Plant_Name ?Fuel_Type ?Country
        where {
        ?Power_Plant_Name a p14:PowerGenerator;
    j.0:realizes ?Fuel_Type.

        ?Power_Plant_Name a p14:PowerGenerator;
    p9:hasAddress ?x.
        ?x system_v1:hasName ?Country.
}`;
var qsCapacity = `    PREFIX system_realization: <http://www.theworldavatar.com/OntoEIP/system_aspects/system_realization.owl#>         
         PREFIX j.2: <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#>

     select distinct ?Power_Plant ?Capacity
       where {                         
         ?Power_Plant a system_realization:DesignCapacity;
         j.2:hasValue ?x.
         ?x j.2:numericalValue ?Capacity.
             }`;


    function getPlantAggregation(callback) {
        var result = {};
        const ppRoot =config.ppFolder;
        getWorldPPChild(function (err, ppraw) {
            if (err) {
                callback(err);
                return;
            }

            //replace uri with disk location
            let pp = xmlParser.uriList2DiskLoc(ppraw, ppRoot);


            // logger.debug(util.inspect(pp));

            async.concat(pp, query, function (err, plantData) {
                //For each point, calculates emission
                if(err){
                    throw err;
                }

                var sum = 0;
                plantData.forEach(function (plantDatum) {
                    plantDatum.emission = calculateEmission(plantDatum.type, plantDatum.capacity);
                 sum+=plantDatum.emission;
                });
                //Calculate Sum
                //Group by countrie
                
                result.dataBycountry = groupBy(plantData, "country");
                result.sum = sum;
                result.getByCountry = function (country) {
                    //TODO: not exist
                    logger.debug(this.dataBycountry[country])
                    return this.dataBycountry[country] ;
                }
                result.countryList = (function () {
                    let list = [];
                    for(let key in result.dataBycountry){
                        if(result.dataBycountry.hasOwnProperty(key)){
                            list.push(key);
                        }
                    }
                 return list;
                })();
                
                result.capBycountrySum = (function () {
                    let sum ={};
                    for(let countrykey in result.dataBycountry){
                        if(result.dataBycountry.hasOwnProperty(countrykey)){
                            sum[countrykey] = {};
                            let capacity = 0;
                            let arrByFuel  =  groupBy(result.dataBycountry[countrykey], "type");
                            for(let fuelType in arrByFuel){
                                if(arrByFuel.hasOwnProperty(fuelType)){
                                    sum[countrykey][fuelType]=0;
                                  //  logger.debug(JSON.stringify(arrByFuel))
                                 arrByFuel[fuelType].forEach(function (item) {
                                     sum[countrykey][fuelType]+=parseFloat(item["capacity"]);
                                 });
                                }
                                }


                        }
                    }
                    return sum;
                })();
                result.convert = function (country, percentage) {
                    //TODO: not exist country
                    //cal new coal capa
                    //cal new coal emisison
                    //cal new ns cap
                    //cal new ns emission
                    //sum up
                    let newCalCap = this.capBycountrySum[country]["http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#CoalGeneration"] * percentage/100 || 0;
                    logger.debug(this.capBycountrySum[country]["http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#CoalGeneration"])
                    let newCalEmi = calculateEmission("http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#CoalGeneration", newCalCap);
                    logger.debug(newCalEmi)

                    let newNSCap = this.capBycountrySum[country]["http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#NaturalGasGeneration"] * (1-percentage/100);
                    logger.debug(newNSCap)
                    let newNSEmi = calculateEmission("http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#NaturalGasGeneration", newNSCap);
                    logger.debug(newNSEmi)
                    return  newCalEmi + newNSEmi;
                }
                callback(null, result);
            })
        });



        var groupBy = function(xs, key) {
            return xs.reduce(function(rv, x) {
                (rv[x[key]] = rv[x[key]] || []).push(x);
                return rv;
            }, {});
        };

        function getWorldPPChild(callback) {
            //loop through world node children , compare type to be PP or not
            fs.readFile(worldNode, function (err, file) {
                if (err) {
                    logger.debug("errReadingFile");
                    callback(err);
                    return;
                }
                const root = xmlParser.parseXMLFile(file);
                var PP = xmlParser.getPPChildren(root);


                logger.debug("children length: "+ PP.length);
                callback(null, PP);
            })
        }



        function query(item, callback) {
            fs.readFile(item.diskLoc, function (err, file) {
                if(err){
                    callback(err);
                return;
                }
                var mparser = new parser.RdfParser({file: file, uri:item.uri});

                async.map([qsTypeCountry, qsCapacity], mparser.mquery.bind(mparser), function (err, result) {
                    if (err) {
                        callback(err);
                        return;
                    }
                    //logger.debug(result);

                    if(result.length ===2 && result[0][0] &&result[0][0]['?Fuel_Type'] &&result[0][0]['?Country'] && result[1][0]['?Capacity']){
                        //logger.debug("Extract: " +  result[0][0]['?Fuel_Type'])
                        callback(null, {uri: item.uri, name: result[0][0]['?Power_Plant_Name']['value'],type: result[0][0]['?Fuel_Type']['value'], country: result[0][0]['?Country']['value'], capacity: result[1][0]['?Capacity']['value']});

                    } else {
                        // logger.debug("!!!!!!!!!!!!!!!!!!!!!!query: "+uri)
                        // logger.debug(result);
                        callback(null,null)
                    }
                });
            });

        }


        function calculateEmission(type, capacity) {
            switch (type) {
                case "http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#CoalGeneration":
                    return capacity * 1000 * 0.8 * 0.001;
                case "http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#NaturalGasGeneration":
                    return capacity * 750 * 0.5 * 0.001;
                default:
                    logger.debug("no such type")

            }
        }


    }






module.exports = {getPlantAggregation};