/**
 * Created by Shaocong on 10/2/2017.
 */
/**
 Test all data retrievers modules, with a  get in their name.
 */

/*******IMPORT**********/
var expect = require("chai").expect;
var util = require("util");
var rdfParser = require("../agents/rdfParserObsolete.js");
var request = require("request");
var path = require("path");


var config = require("../config");

var testNode = path.resolve("./testFiles/E-301.owl");


/*GetLiteral********/
const GetLiteral = require('../agents/GetLiteralData');
describe("get Literal data in a file", function () {
    it('find all literal data of a file with no extra options', function (done) {

        GetLiteral(testNode, function (err, result) {

            console.log("!!!!!!!!!!!!!!!!!")
            console.log(result)
            const expected = [];

            expect(result).to.deep.equal(expected);
            done()
        })

    });

    it('find  literal data with specific names in a file with no extra options', function (done) {

        GetLiteral(testNode, {specificNames: ["V_Pressure_3-1","V_Temperature_3-1" ]},function (err, result) {

            console.log("!!!!!!!!!!!!!!!!!")
            console.log(result)
            const expected = [];

            expect(result).to.deep.equal(expected);
            done()
        })

    });
});



const GetBms =require('../agents/GetBmsDataObsolete')
describe("get BMS data in a file", function () {
    it('find all data in a bms file with no extra options', function (done) {

        const testNode = path.resolve("./testFiles/BMS/HVAV-S7-04_Hstat_sensor1.owl");
        GetLiteral(testNode, function (err, result) {

            console.log("!!!!!!!!!!!!!!!!!")
            console.log(result)
            const expected = [];

            expect(result).to.deep.equal(expected);
            done()
        })

    });
});