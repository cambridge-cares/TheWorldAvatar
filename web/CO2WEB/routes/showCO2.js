/**
 * Created by Shaocong on 5/25/2017.
 */
var express = require('express');
var router = express.Router();
var rdfParser = require("../rdfParser.js");

let fileFolder = require("../config.js").root;

let fileUrl = fileFolder + "/JurongIsland.owl";

router.get('/', function (req, res) {
console.log(fileUrl);
    console.log(fileUrl);

    let opts = {//format opts to feed in rdfParser
        fileUrl : fileUrl,
        uri :'http://www.theworldavatar.com/JurongIsland.owl'


    };

    let node ="http://www.jparksimulator.com/JurongIsland.owl#V_CO2_Jurong"; // individual
    let property = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue";//property

    let parser = rdfParser(opts);
        let result = parser.search(node, property);//search in parsed graph for this property of this note
        console.log(result);
        result = parseFloat(result).toFixed(4);//format result into float like 1.0000
        res.render('co2', { co2Value: result }); //render the view with this value


});

module.exports = router;