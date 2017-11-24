/**
 * Router: map of b3
 * @type {*}
 */


var express = require('express');

const routerFact = require("./routerFact/mapRouterFactCached"),
    getB3Coordi = require("../agents/GetB3Coordi");
let router = express.Router();

 router = routerFact(router, getB3Coordi, {title:"BiodeselPlant3 Map", subtitle:"BiodeselPlant3 Map"}, "mapB3");

module.exports = router;

