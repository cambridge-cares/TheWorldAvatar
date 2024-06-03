/**
Aggregate plant emission by type and country[commented]

 */

//////////////////////////////////////////////////////////////////////////////////////////////////
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

const config =  require('../config');
const worldNode = config.ppNode;
const xmlParser = require("./fileConnection2Way");
const parser = require('./rdfParser');
const async = require('async');
const fs =require('graceful-fs')
const util = require('util');
const path = require('path')
const processor = Object.create(xmlParser);
const pprefix = "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#";
//////////////////////////////////////////////////////////////////////////////////////////////////

//sparql query////////////////////////////////////////////////////////////////////////////////////
var qsTypeCountry = `
        PREFIX system_v1: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#>
     PREFIX j.0: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
     PREFIX p14: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
          PREFIX p9: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>

        select distinct ?Power_Plant_Name ?Fuel_Type ?Country
        where {
        ?Power_Plant_Name a p14:PowerPlant ;
    j.0:realizes ?Fuel_Type.

        ?Power_Plant_Name a p14:PowerPlant ;
    p9:hasAddress ?Country.
}`;
var qsCapacity = `    PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>         
         PREFIX j.2: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>

     select distinct ?Power_Plant ?Capacity
       where {                         
         ?Power_Plant a system_realization:DesignCapacity;
         j.2:hasValue ?x.
         ?x j.2:numericalValue ?Capacity.
             }`;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

     /*
     main function
     **/
    function getPlantAggregation(callback) {


        var result = {};
        getWorldPPChild(function (err, ppraw) {
            if (err) {
                callback(err);
                return;
            }
            //replace uri with disk location
            let pp = xmlParser.uriList2DiskLoc(ppraw, config.root);

            async.concat(pp, query, function (err, plantDataO) {
                //For each point, calculates emission
                if(err){
                    throw err;
                }

                
                var sum = 0;
                let plantData = []
                plantDataO.forEach((item)=>{if(item){plantData.push(item)}})
                
                plantData.forEach(function (plantDatum) {
					if(plantDatum){
                    plantDatum.emission = calculateEmission(plantDatum.type, parseFloat(plantDatum.capacity));
					//console.log(plantDatum.emission);
                 sum+=plantDatum.emission;
					}
                });
                //Calculate Sum
                //Group by countrie
                
                result.dataBycountry = groupBy(plantData, "country");
                result.sum = sum;
                result.getByCountry = function (country) {
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
                 return list.sort();
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
                /**
                convert coal to natural gass and calculated new emission
                input:
                country: country id
                percentage: percentage of convertion

                return new emission value
                ***/
                result.convert = function (country, percentage) {
                    //cal new coal capa
                    //cal new coal emisison
                    //cal new ns cap
                    //cal new ns emission
                    //sum up
                    if(!this.capBycountrySum[country] || !this.capBycountrySum[country][pprefix+"CoalGeneration"]){
                         return 0;
                    }

                    let oldCalCap = this.capBycountrySum[country][pprefix+"CoalGeneration"];
                    let newCalCap = this.capBycountrySum[country][pprefix+"CoalGeneration"] * (1-percentage/100) || 0;
                    let oldNSCap = this.capBycountrySum[country][pprefix+"NaturalGasGeneration"];
                    let oldEmission = calculateEmission(pprefix+"CoalGeneration", oldCalCap)+calculateEmission(pprefix+"NaturalGasGeneration", oldNSCap);

                    let newCalEmi = calculateEmission(pprefix+"CoalGeneration", newCalCap);
                    logger.debug(newCalEmi);

                    let newNSCap = oldNSCap+ newCalCap - oldCalCap;
                    logger.debug(newNSCap);
                    let newNSEmi = calculateEmission(pprefix+"NaturalGasGeneration", newNSCap);
                    logger.debug(newNSEmi);

                    return oldEmission - newCalEmi - newNSEmi ;
                }
                callback(null, result);
            })
        });



         /**
         utility function:
         group a map by certain key
         input xs => map
         key => key to group by
         return new map grouped by key

         **/
        var groupBy = function(xs, key) {
            return xs.reduce(function(rv, x) {
                (rv[x[key]] = rv[x[key]] || []).push(x);
                return rv;
            }, {});
        };


        /***
        async function to get all powerplant uri from root file
        ****/
        function getWorldPPChild(callback) {
            //loop through world node children , compare type to be PP or not
            processor.init({});
    
            processor.doConnect(worldNode,0).then((PPchildren)=>{
			
                var PP =  PPchildren.map((item)=> normURI(item['target']));
				//console.log('CO2PlantAggregation: found plant')
				//console.log(PP)
                callback(null, PP);
            })
        }


        function normURI(uri){
			let sec = uri.split('#');
			return sec[0];
		}

        /**

          async function to query all powerlant file for the capacity, type and contry info
          return: [{uri, name, type,country, capacity}]
        ***/
        function query(item, callback) {
			///console.log('read: '+item.diskLoc);
            fs.readFile(item.diskLoc, function (err, file) {
                if(err){
					console.log(err);
                    callback(null,null);
                return;
                }
                var mparser = new parser.RdfParser({file: file, uri:item.uri});

                async.map([qsTypeCountry, qsCapacity], mparser.mquery.bind(mparser), function (err, result) {
                    if (err) {
                        callback(err);
                        return;
                    }
                    //logger.debug(result);
                      //  console.log('query result:' + result);

                    if(result.length ===2 && result[0][0] &&result[0][0]['?Fuel_Type'] &&result[0][0]['?Country'] && result[1][0]['?Capacity']){
                        //logger.debug("Extract: " +  result[0][0]['?Fuel_Type'])
                        callback(null, {uri: item.uri, name: result[0][0]['?Power_Plant_Name']['value'],type: result[0][0]['?Fuel_Type']['value'], country: result[0][0]['?Country']['value'], capacity: result[1][0]['?Capacity']['value']});

                    } else {
                        // logger.debug("!!!!!!!!!!!!!!!!!!!!!!query: "+uri)
                        callback(null,null)
                    }
                });
            });

        }


         /***
         emission equation

         input:
         type: type of fuel
         capacity: capacity value
         return : emission value
         ***/
        function calculateEmission(type, capacity) {
            switch (type) {
                case "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CoalGeneration":
                    return capacity * 1000 * 0.8 * 0.001;
                case "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGasGeneration":
                    return capacity * 750 * 0.5 * 0.001;
					                case "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#OilGeneration":
                    return capacity * 750 * 0.5 * 0.001;
                default:
                    logger.debug("no such type");
					return 0;

            }
        }


    }


/**
getPlantAggregation((err, result)=>{
    "use strict";
    if(err) {console.log(err)};
    console.log('print result')
    console.log(result)
})

**/
module.exports = {getPlantAggregation};