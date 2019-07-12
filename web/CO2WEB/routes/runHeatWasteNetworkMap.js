

const routerFact = require("./routerFact/mapRouterFact");
var express= require('express')
var router = express.Router()
var runHeatWasteNetwork = require("../agents/RunHeatWasteNetwork")

var config = require('../config')
var LiteralData = require('../agents/GetLiteralData')

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


function  getInitialHWData(cb) {
    let names = [
        "SourcePlant1Quality",
        "SourcePlant2Quality",
        "SourcePlant3Quality",
        "SourcePlant4Quality",
        "SourcePlant5Quality",
        "SinkPlant1Quality",
        "SinkPlant2Quality",
        "SinkPlant3Quality",
        "SinkPlant4Quality",
        "SinkPlant5Quality",
        "SourcePlant1Quantity",
        "SourcePlant2Quantity",
        "SourcePlant3Quantity",
        "SourcePlant4Quantity",
        "SourcePlant5Quantity",
        "SinkPlant1Quantity",
        "SinkPlant2Quantity",
        "SinkPlant3Quantity",
        "SinkPlant4Quantity",
        "SinkPlant5Quantity",

    ];

    LiteralData(function (err, result) {
        if(err){
            cb(err);
            return;
        }

        console.log(result);
        cb(null, result); //render the view with this value

    },config.heatWasteNode, {specificNames:names})


}



router.get("/initial", (req, res, next)=>{

    getInitialHWData((err, result)=>{
        if(err) {
            console.log(err)
            next(err)
            return;
        }
        console.log(result)

        let packed = {};

        result.forEach((item, idx)=>{

            if(idx < 5){

                packed.SourcePlantQualities = packed.SourcePlantQualities?packed.SourcePlantQualities:[];
                packed.SourcePlantQualities.push(item.value);
            }else if(idx < 10){

                packed.SinkPlantQualities = packed.SinkPlantQualities?packed.SinkPlantQualities:[];
                packed.SinkPlantQualities.push(item.value);
            }else if (idx < 15){
                packed.SourcePlantQuantities = packed.SourcePlantQuantities?packed.SourcePlantQuantities:[];
                packed.SourcePlantQuantities.push(item.value);
            }else {
                packed.SinkPlantQuantities = packed.SinkPlantQuantities?packed.SinkPlantQuantities:[];
                packed.SinkPlantQuantities.push(item.value);
            }

        })

        console.log(packed)
        res.json(packed);
    })
})










//TODO: return simulation result when requested
//TODO: use cache
router.post("/simulation", (req, res, next)=>{
    if(!req.body) {
        next(new Error("Can not find req body"));
    }
    let parsedBody = JSON.parse(req.body);

    if(!parsedBody.plantData){
        next(new Error("Can not find plant data"));

    }

    runHeatWasteNetwork(parsedBody.plantData , (err, result)=>{
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