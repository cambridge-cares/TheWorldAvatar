/**
 * Created by Shaocong on 10/6/2017.
 */
const routerFact = require("./mapRouterFact");
var getAltPPcoordi = require('../agents/ReadAltPPCoordi');

console.log(getAltPPcoordi)
var router = routerFact(getAltPPcoordi, {title:"Powerplant Map", subtitle:"Powerplant Map"}, "mapNuclear");

module.exports = router;