/**
 * Router: plot of BMS
 * corresponding agent: GetAllSensor(get initial sensor list)
 * @type {*}
 */

var express = require('express');
var router = express.Router();

var config = require("../config");

var getAllSensor = require("../agents/GetAllSensor");

var cacheRouter  = require("../agents/Cache");

function  sendResult(result, res) {
    res.render('bmsPlot', {sensorList :JSON.parse(result)}); //render the view with this value
}

router = cacheRouter(router).get('/', getAllSensor, { sendResult, expiredTime:36000000});


module.exports = router;