
var express = require('express');
var router = express.Router();


const CO2PPlantSum = require('../agents/CO2PlantAggregation');

let CO2Dataset;
if(!CO2Dataset){
    CO2PPlantSum.getPlantAggregation(function (err, result) {
        if(err){
            //next(err);
            console.log(err);
            throw(err);
            return;
        }
        CO2Dataset = result;

    });
}


router.get('/', function (req, res, next) {
        if(CO2Dataset) {
            res.render('PPco2', {co2Value: parseFloat(CO2Dataset.sum).toFixed(), countries: CO2Dataset.countryList}); //render the view with this value
        } else{
            next(new Error("Server can not provide the data now"))
        }
});

router.post('/listbycountry', function (req, res, next) {
    if(CO2Dataset) {
        if(!req.body) {
            next(new Error("Can not find req body"));
        }
        console.log(req.body);
        let parsedBody = JSON.parse(req.body)

        console.log(CO2Dataset.getByCountry(parsedBody.country))
        res.json(CO2Dataset.getByCountry(parsedBody.country)); //render the view with this value
    } else{
        next(new Error("Server can not provide the data now"))
    }

});

router.post('/convertion', function (req, res,next) {
    if(CO2Dataset) {
        if(!req.body) {
            next(new Error("Can not find req body"));
        }
        console.log(req.body);
        let parsedBody = JSON.parse(req.body)

        console.log(CO2Dataset.convert(parsedBody.country, parsedBody.percent))
        res.json(CO2Dataset.convert(parsedBody.country, parseFloat(parsedBody.percent))); //render the view with this valu
    } else{
        next(new Error("Server can not provide the data now"))
    }


});
module.exports = router;