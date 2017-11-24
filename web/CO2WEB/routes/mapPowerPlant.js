
const routerFact = require("./routerFact/mapRouterFactCached");
var getPPcoordi = require('../agents/ReadPowerPlantCoordi');

var express= require('express')
var router = express.Router()
router = routerFact(router, getPPcoordi, {title:"Powerplant Map", subtitle:"Powerplant Map"}, "mapPP");

module.exports = router;