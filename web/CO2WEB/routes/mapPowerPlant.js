/**
 * Created by Shaocong on 9/7/2017.
 */

var express = require('express');
var router = express.Router();
var getPPcoordi = require('../agents/ReadPowerPlantCoordi');
let PPcoordi;
if(!PPcoordi){
  getPPcoordi(function (err, result) {
      if(err){
          throw err;
      }
      PPcoordi = result;
  })
}

router.get('/', function(req, res, next) {
if(!PPcoordi){
    next(new Error("Server can not provide the data now"))
} else{
    res.render("mapPP", {coordinates: PPcoordi})
}
    //send a file
});

module.exports = router;
