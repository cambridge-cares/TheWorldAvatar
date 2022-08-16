const routerFact = require("./routerFact/mapRouterFactCached");
var getEngPPcoordi = require('../agents/getEnglandPPcoordinates');
var express= require('express')
var router = express.Router()

router = routerFact(router, getEngPPcoordi,
    { title: "UK Digital Twin", subtitle: "UK power plant map", description: "This is a UK power plant map including 1152 plants" }, "ontoTwinUK"); //reference to the view pug, not the js file found in \powersys\javascripts

module.exports = router;