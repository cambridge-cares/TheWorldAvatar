
const routerFact = require("./routerFact/mapRouterFactCached");
var getAltPPcoordi = require('../agents/ReadAltPPCoordi');
var express= require('express')
var router = express.Router()
let RunGAMS = require('../agents/RunGAMSPredefinedOwlFile')
 router = routerFact(router, (cb)=> cb(null, {})
 , {title:"Powerplant Map", subtitle:"Powerplant Map"}, "mapPPAlt");

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


    RunGAMS(parsedBody.predefinedId, function (err, result) {
        if(err){
            console.log("GAMS run error")

            next(new Error("GAMS run error"));
            return
        }
        res.json(result);
    })

})
module.exports = router;