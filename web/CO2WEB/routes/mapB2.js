var express = require('express');

const routerFact = require("./routerFact/mapRouterFactCached"),
    getCoordi = require("../agents/GetCoordi");
const config = require("../config")
let router = express.Router();

router = routerFact(router, getCoordi.bind(null, config.jurongNode), {title:"BiodeselPlant3 Map", subtitle:"BiodeselPlant3 Map"}, "mapB3");

module.exports = router;

