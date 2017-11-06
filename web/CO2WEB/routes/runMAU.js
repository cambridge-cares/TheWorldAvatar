
var express = require('express');
var router = express.Router();
var MAU = require("../agents/RunMAU");

router.use(function(req, res, next) {
    res.header("Access-Control-Allow-Origin", "*");
    res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
    next();
});

router.get('/', function (req, res, next) {
    if(!req.body) {
        next(new Error("Can not find req body"));
    }

    console.log("query:")
    console.log(req.query.input);
    let inputStr = req.query.input;
    if(!inputStr){
        res.status(400);
        return;
    }
    let inputLists = inputStr.trim().split(' ');
console.log(inputLists)
    inputLists= inputLists.map((numStr)=>{return JSON.parse(numStr)})
    console.log(inputLists)
    let mMAU = new MAU();

    results = inputLists.map((inputs)=>{
        return  mMAU.runSimulation(inputs);

    })
    console.log("MAU simulation result:")
    console.log(results);

    res.json(results);
});

module.exports = router;