/**
Testing the rdfParser, which extracts o by providing s and p in a s,p,o triple defined in owl file
 */

/*******IMPORT**********/
var expect = require("chai").expect;
var util = require("util");
var rdfParser = require( "../../agents/rdfParser.js");
var request = require("request");
var path = require("path");


var config = require("../../config");

var opts = {
    fileUrl: config.root + "/JurongIsland.owl",
    uri: 'http://www.theworldavatar.com/JurongIsland.owl'
};

describe('RDFParser: ', function () {

    it('could find the value of property of an indivdiual in a specific file', function () {


        let node = 'http://www.jparksimulator.com/JurongIsland.owl#V_CO2_Jurong';
        let property = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue";
        let result = new rdfParser.RdfParser(opts).search(node, property);
        console.log(util.inspect(result));

        expect(result.value).to.equal('1500.0');

    })


});
