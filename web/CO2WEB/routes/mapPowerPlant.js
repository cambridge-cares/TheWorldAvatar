
/**
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
      console.log("done loading pp coordis")

  })
}

router.get('/', function(req, res, next) {
    res.render("mapPP");
    //send a file
});


router.get('/coordinates', function (req, res, next) {

    if(!PPcoordi){
        next(new Error("Server can not provide the data now"))
    } else{
        res.json(PPcoordi);
    }});
module.exports = router;
***/

/**
 * Created by Shaocong on 9/21/2017.
 */
const routerFact = require("./mapRouterFact");
var getPPcoordi = require('../agents/ReadPowerPlantCoordi');


var router = routerFact(getPPcoordi, {title:"Powerplant Map", subtitle:"Powerplant Map"}, "mapPP");

module.exports = router;