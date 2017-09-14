/**
 * Created by Shaocong on 6/15/2017.
 */

var express = require('express');
var router = express.Router();

var config = require("../config");
/* GET home page. */

router.get("/", function (req, res, next) {

    console.log("get /bmsplot");
   // console.log(config.viewRoot+'/bmsPlot.html')
   // res.sendFile( config.viewRoot+'/bmsPlot.html');
    //get BMS data
    //TODO l sent initial data here
    //todo HOW ? through event=>socket/ through socket directly/initial render



try{
    res.render('bmsPlot'); //render the view with this value

}catch (err){
    next(err);
}

    //res.render('co2', { co2Value: 1 }); //render the view with this value

});

module.exports = router;