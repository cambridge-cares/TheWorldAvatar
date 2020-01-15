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

router.get('/', function(req, res, next) {

	    getAllSensor(function (err, result) {
        if(err){
            console.log("can not get sensor list backend error")

            next(new Error("can not get sensor list backend error"));
            return
        }
        console.log(result)
    res.render('bmsPlot', {sensorList :result}); //render the view with this value
    })
})



module.exports = router;