
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


var config = require("../config");

var cacheRouter  = require("../agents/Cache");
var LiteralData = require("../agents/GetLiteralData");

var app = express();

cacheRouter(router).get('/', LiteralData, {req2args, sendResult});

app.use('getLiteralData', router);

before(function () {//open server before all tests
    app.listen(3000, function () {
        console.log('Server listening on port 3000');
    });
});
const url = "http://localhost:3000/getLiteralData";
describe("Wrap around router to implement normal/cache handler for one path", function () {
    it('get normally', function (done) {
        request.get(url).on('response', function(response) {
            console.log(response.body);
         done()
        })
    });
    it('get with cache', function (done) {
        request.get(url).on('response', function(response) {
            console.log(response.body);
               done()
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
