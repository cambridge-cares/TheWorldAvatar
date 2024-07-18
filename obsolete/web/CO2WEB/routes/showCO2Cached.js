/**
 * A cached version of showCO2
 */

var express = require('express');
var router = express.Router();
var cacheRouter  = require("../agents/Cache");

const co2Add = require('../agents/CO2Aggregation');

router = cacheRouter(router).get('/', co2Add, { sendResult, expiredTime:360000000});


function sendResult(result,res) {
    res.render('co2', { co2Value: result }); //render the view with this value

}

module.exports = router;