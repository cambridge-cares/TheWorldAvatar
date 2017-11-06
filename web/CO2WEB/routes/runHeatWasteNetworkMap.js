

const routerFact = require("./mapRouterFact");
var express= require('express')
var router = express.Router()
var runHeatWasteNetwork = require("../agents/runHeatWasteNetwork")



 function getHWPPcoordi(cb){
    cb(null, [{uri:"", location:{lat: 1.260414, lng: 103.676268}},
        {uri:"", location:{lat: 1.267866, lng: 103.693315}},
        {uri:"", location:{lat: 1.282279, lng: 103.714007}},
     {uri:"", location:{lat: 1.263889, lng: 103.668609}},
         {uri:"", location:{ lat: 1.273710, lng: 103.675132}}
     ])
 }

router = routerFact(router, getHWPPcoordi, {title:"Powerplant Map", subtitle:"Powerplant Map"}, "mapHW");
//when get simulation request, init calculation and retrieve result


//TODO: return simulation result when requested
//TODO: use cache
router.get("/simulation", (req, res, next)=>{

    runHeatWasteNetwork((err, result)=>{
        if(err) {
            console.log(err)
            next(err)
            return;
        }
        console.log(result)

        res.json(result);
    })
})
module.exports = router;