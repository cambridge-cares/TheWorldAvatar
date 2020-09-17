const routerFact = require("./routerFact/mapRouterFactCached");
var getEngPPcoordi = require('../agents/getEnglandPPcoordinates');
var getPowerPlantAttr = require('../agents/getPowerPlantAttr');
var express= require('express')
var router = express.Router()

router = routerFact(router, getEngPPcoordi, getPowerPlantAttr,
    { title: "UK Digital Twin", subtitle: "UK power plant map", description: "This is a UK power plant map including 1152 plants" }, "ontoTwinUK"); //reference to the view pug, not the js file found in \javascripts

module.exports = router;