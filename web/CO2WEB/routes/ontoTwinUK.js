const routerFact = require("./routerFact/mapRouterFactCached");
var getAltPPcoordi = require('../agents/ReadAltPPCoordi');
var express= require('express')
var router = express.Router()
 router = routerFact(router, (cb)=> cb(null, {})
 , {title:"UK OntoTwin", subtitle:"UK OntoTwin"}, "ontoTwinUK"); //reference to the pug, not the js file found in \javascripts

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
module.exports = router;