/**
 * Router: plot of BMS
 * corresponding agent: GetAllSensor(get initial sensor list)
 * @type {*}
 */

var express = require('express');
var router = express.Router();

var config = require("../config");

var getAllSensor = require("../agents/GetAllSensor");
router.get('/', function (req, res, next) {
getAllSensor(function(err,result){
	    res.render('bmsPlot', {sensorList :result}); //render the view with this value

})
})

module.exports = router;