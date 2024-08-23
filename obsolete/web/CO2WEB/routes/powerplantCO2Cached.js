/**
 * Cached version of powerplantCO2
 */

var express = require('express');
var router = express.Router();

const cacheRouter  = require("../agents/Cache");

const CO2PPlantSum = require('../agents/CO2PlantAggregation');

let CO2Dataset;


function  getCO2Dataset(cb) {
    console.log("getCO2DATASET")
    if(!CO2Dataset){
        console.log("initCO2 DATASET")
        CO2PPlantSum.getPlantAggregation(function (err, result) {
            if(err){
                //next(err);
                console.log(err);
                cb(err);
            }
            CO2Dataset = result;
            console.log("Dataset: ")
            console.log(CO2Dataset)
           cb(null, CO2Dataset)
        });
    } else{
        cb( null, CO2Dataset)
    }
}

 var getSumNCountries = getDataFromCO2Dataset.bind(null, (CO2Dataset)=>{
    return {co2Value: parseFloat(CO2Dataset.sum).toFixed(), countries: CO2Dataset.countryList} });

var getListByCountry = getDataFromCO2Dataset.bind(null, (CO2Dataset, country)=>{return CO2Dataset.getByCountry(country) });

var getConvertionResult = getDataFromCO2Dataset.bind(null,(CO2Dataset, country, percent)=>{
    return CO2Dataset.convert(country, percent)
} )
router = cacheRouter(router).get('/', getSumNCountries, {expiredTime:30000000, sendResult:sendRender});

router = cacheRouter(router).post('/listbycountry', getListByCountry, {expiredTime:30000000, req2args: getCountryName,sendResult});

router = cacheRouter(router).post('/convertion', getConvertionResult, {expiredTime:30000000, req2args: getCountryNameNPer,sendResult});


function getDataFromCO2Dataset(formatter, cb, ...args) {
    console.log("data from co2")
    getCO2Dataset((err, CO2Dataset)=>{
        if(err) {
            cb(err);return
        }
       // console.log("formatted data: ")
        //console.log(formatter(CO2Dataset, ...args))
        cb(null, formatter(CO2Dataset, ...args))

    })
}

function sendResult(result,res) {
    res.json(JSON.parse(result))
}

 function sendRender(result,res){
     res.render('PPco2', JSON.parse(result)); //render the view with this value
    }

function getCountryName(req){
    if(!req.body) {
        return new Error("Can not find req body");
    }
    let parsedBody = JSON.parse(req.body);
    console.log("!!!!!!!!!!!!!!!!parsed body:")
    console.log(parsedBody)

      return parsedBody.country
}

function  getCountryNameNPer(req) {
    if(!req.body) {
        return new Error("Can not find req body");
    }
    let parsedBody = JSON.parse(req.body);

    return [parsedBody.country, parseFloat(parsedBody.percent)]
}

module.exports = router;