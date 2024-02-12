
/**
 Test cache module
 */

/*******IMPORT**********/
var expect = require("chai").expect;
var util = require("util");
var request = require("request");
var path = require("path");
var express = require('express');
var router = express.Router();
var bodyParser = require('body-parser');


var config = require("../config");

var cacheRouter  = require("../agents/Cache");
var LiteralData = require("../agents/GetLiteralData");

var app = express();

router = cacheRouter(router).post('/', LiteralData, {req2args, sendResult});

app.use(bodyParser.text({ type: 'application/json' }));

app.use('/getData', router);

before(function () {//open server before all tests
    app.listen(3000, function () {
        console.log('Server listening on port 3000');
    });
});
const url = "http://localhost:3000/getData";
const nodeLoc = "http://www.theworldavatar.com/E-301.owl"
describe("Wrap around router to implement normal/cache handler for one path", function () {
    it('get normally', function (done) {
        request({method:'POST', url : url, headers:{ "content-type": "application/json"},body:JSON.stringify({"uri":nodeLoc})}, function(error, response, body) {
            if(error){
                console.log(error);
                done(error);
            }
            console.log(JSON.parse(body));
            var result = [{"name":"V_Pressure_3-1","value":"1.0","p":"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue","datatype":"float","unit":"bar"},{"name":"V_Temperature_3-1","value":"30.0","p":"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue","datatype":"float","unit":"Celsius"},{"name":"V_molarF_3-1","value":"30.0","p":"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue","datatype":"float","unit":"kmol/hr"},{"name":"ValueOfHeatDutyOfE-301","value":"510.71283","p":"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue","datatype":"float","unit":"kW"},{"name":"ValueOf_EquipmentCost_E-301","value":"63800.0","p":"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue","datatype":"float","unit":"USD"},{"name":"ValueOf_x_E-301","value":"1.15414725598235E7","p":"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue","datatype":"decimal","unit":"m"},{"name":"ValueOf_y_E-301","value":"140096.192340398","p":"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue","datatype":"decimal","unit":"m"}]

            //TODO: why do I have to parse twice...
            let parsed = JSON.parse(JSON.parse(body))
            expect(parsed).to.deep.equal(result);
            //TODO:
            done();

        })
    });

});


function req2args(req) {
    if(!req.body) {
        return new Error("Can not find req body");
    }
    let parsedBody = JSON.parse(req.body);
    let uri = parsedBody.uri;

    if(!uri){
        console.log("Can not find uri")
        return new Error("Can not find uri in req body")
    }

    return uri;
}

function sendResult(result,res) {
    res.json(result);
}
