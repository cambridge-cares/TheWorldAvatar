
var express = require('express');
var router = express.Router();

const co2Add = require('../agents/CO2Aggregation');





router.get('/', function (req, res) {

    co2Add(function (err, result) {
        result = parseFloat(result).toFixed(4);//format result into float like 1.0000
        res.render('co2', { co2Value: result }); //render the view with this value
    })




});

module.exports = router;