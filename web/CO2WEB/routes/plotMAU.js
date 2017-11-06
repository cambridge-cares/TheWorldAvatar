

var express = require('express');
var router = express.Router();

var config = require("../config");
/* GET home page. */

var getAllSensor = require("../agents/GetAllSensor");

var sensorList;

getAllSensor(function (err, data) {

    sensorList = data;
})

router.get("/", function (req, res, next) {


    res.render('MAUSimPlot'); //render the view with this value

});

module.exports = router;