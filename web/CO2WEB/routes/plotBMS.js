

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

   // console.log(config.viewRoot+'/bmsPlot.html')
   // res.sendFile( config.viewRoot+'/bmsPlot.html');
    //get BMS data
try{
    if(sensorList) {
        res.render('bmsPlot', {sensorList :sensorList}); //render the view with this value
    } else{
        next(new Error("Server has not finished loading data. Please try again."));
    }
}catch (err){
    next(err);
}

    //res.render('co2', { co2Value: 1 }); //render the view with this value

});

module.exports = router;