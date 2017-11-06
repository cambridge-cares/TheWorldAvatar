var express = require('express');

const routerFact = require("./mapRouterFact"),
    getB3Coordi = require("../agents/GetB3Coordi");
let router = express.Router();

 router = routerFact(router, getB3Coordi, {title:"BiodeselPlant3 Map", subtitle:"BiodeselPlant3 Map"}, "b3map");

module.exports = router;

