
var express = require('express');
var router = express.Router();

const co2Add = require('../agents/CO2Aggregation');



router.get('/', function (req, res, next) {
    co2Add(function (err, result) {
        if(err){
			console.log("co2ADD Module err: "+err);
          next(err);
         return;
        }

        res.render('co2', { co2Value: result }); //render the view with this value
    })

});

module.exports = router;