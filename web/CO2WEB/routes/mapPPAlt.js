
const routerFact = require("./routerFact/mapRouterFactCached");
var getAltPPcoordi = require('../agents/ReadAltPPCoordi');
var express= require('express')
var router = express.Router()
console.log(getAltPPcoordi)
 router = routerFact(router, getAltPPcoordi, {title:"Powerplant Map", subtitle:"Powerplant Map"}, "mapPP");

module.exports = router;