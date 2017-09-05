/**
 * Created by Shaocong on 8/31/2017.
 */

const config =  require('../config');
const worldNode = config.worldNode;
const fileConnection = require("./fileConnection");
const parser = require('../util/rdfParser');
const $rdf = require('rdflib');
const async = require('async');
const fs =require('fs')
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
        const ppRoot = path.join(config.root, "powerplants");
        getWorldPPChild(function (err, ppraw) {
            if (err) {
                callback(err);
                return;
            }

            //console.log(util.inspect(ppraw));

            //replace uri with disk location
            let pp = ppraw.map(function (item) {
               // console.log("map:"+item)

                item = item.replace("http://www.theworldavatar.com",ppRoot);

                item = item.replace("http://www.jparksimulator.com",ppRoot);

                return item
            })

           // console.log(util.inspect(pp));

            async.concat(pp, query, function (err, plantData) {
                //For each point, calculates emission
                if(err){
                    throw err;
                }
               // console.log("final result:!!!!!!!!!!!!!!!!!!!!")
               // console.log(plantData.length);
               // console.log(plantData)


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
                    console.log(this.dataBycountry[country])
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
                                  //  console.log(JSON.stringify(arrByFuel))
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
                    let newCalCap = this.capBycountrySum[country]["http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#CoalGeneration"] * percentage/100;
                    console.log(this.capBycountrySum[country]["http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#CoalGeneration"])
                    let newCalEmi = calculateEmission("http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#CoalGeneration", newCalCap);
                    console.log(newCalEmi)

                    let newNSCap = this.capBycountrySum[country]["http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#NaturalGasGeneration"] * (1-percentage/100);
                    console.log(newNSCap)
                    let newNSEmi = calculateEmission("http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#NaturalGasGeneration", newNSCap);
                    console.log(newNSEmi)
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
                    console.log("errReadingFile");
                    callback(err);
                    return;
                }
                const root = fileConnection.parseXMLFile(file);
                var children = fileConnection.getChildren(root);
                console.log("children length: "+ children.length);

                //TODO: the type info should be wrapped in getChildren function
                var PP = children.filter(function (uri) {
                    return isAPP(uri);
                });

                console.log("children length: "+ PP.length);
                callback(null, PP);
            })
        }

        //TODO: for each child, read file, query, sum up result
        function isAPP(url) {
            let loweUrl = url.toLowerCase();
            return loweUrl.includes("power_plant") || loweUrl.includes("coal") ||loweUrl.includes("oil") ||loweUrl.includes("gas")||loweUrl.includes("power_station");
        }


        function query(uri, callback) {
            var mparser = new parser.RdfParser({fileUrl: uri});
            //console.log(util.inspect(mparser));

            async.map([qsTypeCountry, qsCapacity], mparser.mquery.bind(mparser), function (err, result) {
                if (err) {
                    callback(err);
                    return;
                }
                 //console.log(result);

                if(result.length ===2 && result[0][0] &&result[0][0]['?Fuel_Type'] &&result[0][0]['?Country'] && result[1][0]['?Capacity']){
                    //console.log("Extract: " +  result[0][0]['?Fuel_Type'])
                    callback(null, {uri: uri, name: result[0][0]['?Power_Plant_Name']['value'],type: result[0][0]['?Fuel_Type']['value'], country: result[0][0]['?Country']['value'], capacity: result[1][0]['?Capacity']['value']});

                } else {
                   // console.log("!!!!!!!!!!!!!!!!!!!!!!query: "+uri)
                   // console.log(result);
                    callback(null,null)
                }
            });
        }


        function calculateEmission(type, capacity) {
            switch (type) {
                case "http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#CoalGeneration":
                    return capacity * 1000 * 0.8 * 0.001;
                case "http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#NaturalGasGeneration":
                    return capacity * 750 * 0.5 * 0.001;

                default:
                    console.log("no such type")

            }
        }


    }






module.exports = {getPlantAggregation};