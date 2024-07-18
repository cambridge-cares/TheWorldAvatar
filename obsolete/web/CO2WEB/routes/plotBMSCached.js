/**
 * Router: plot of BMS
 * corresponding agent: GetAllSensor(get initial sensor list)
 * @type {*}
 */

var express = require('express');
var router = express.Router();

var config = require("../config");

var getAllSensor = require("../agents/GetAllSensorBg");
var getTSData = require("../agents/GetTSData");

router.get('/', function (req, res, next) {
getAllSensor(function(err,result){
    if (err) {next(err);
    return;
    }
    console.log('got list');
    console.log(result);
	    res.render('bmsPlot', {sensorList :result}); //render the view with this value

})
})
router.post('/initSensor', function (req, res, next) {
    if(!req.body) {
        next(new Error("Can not find req body"));
    }
    console.log(req.body);
    let parsedBody = JSON.parse(req.body);
    console.log(parsedBody);
    let sensorIRI = parsedBody.sensor;
    getTSData([sensorIRI], {}, function (err, tsdata) {
        if (err) {next(err);
            return;
        }
        res.json(tsdata);
    })
})

module.exports = router;