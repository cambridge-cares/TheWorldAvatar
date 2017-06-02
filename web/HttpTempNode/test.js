/**
 * Created by Shaocong on 5/3/2017.
 */
/**
 * Created by Shaocong on 5/3/2017.
 */
var expect = require("chai").expect();
var db = require("./util/mockDB");
var fileURL = __dirname + "/ROOT" +"/JurongIsland.owl";

var fs = require("fs");
var  rdfParser = require("./util/rdfParser.js");
var util = require("util");
describe('db', function () {

    it("delete statements", function (done) {


        try{

            opts = {
                fileUrl: fileURL
            };

            var parser  = rdfParser(opts);

            var OWL = parser.namespace("http://www.w3.org/2002/07/owl#");
            var EcoIndPark = parser.namespace("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/eco-industrialPark.owl#");

            let nodeIRI = "";
            let results = parser.search(OWL("CM_BiodieselPlant-1"), EcoIndPark(hasIRI));
            util.inspect(results);

        }catch(err){


        }

    })
});