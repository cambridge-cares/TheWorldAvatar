const routerFact = require("./routerFact/mapRouterFactCached");
var getEngPPcoordi = require('../agents/getEnglandPPcoordinates');
var express= require('express')
var router = express.Router()

router = routerFact(router, getEngPPcoordi, {title:"UK Digital Twin", subtitle:"UK power plant map", description:"This is a UK power plant map including 1152 plants"}, "ontoTwinUK"); //reference to the view pug, not the js file found in \javascripts

/**
 router = routerFact(router, (cb)=> cb(null, {})
 , {title:"UK OntoTwin", subtitle:"UK OntoTwin"}, "ontoTwinUK"); //reference to the pug, not the js file found in \javascripts

/**
router.post("/simulation", (req, res, next)=>{
    if(!req.body) {
        console.log("Can not find req body")
        next(new Error("Can not find req body"));
        return;
    }
    let parsedBody = JSON.parse(req.body);

    if(parsedBody.predefinedId === null){
        console.log("Can not find plant data")

        next(new Error("Can not find plant data"));
        return

    }

})
**/
module.exports = router;