/**
 */

/**
An individual server with one cached path that could be used for testing.
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

    app.listen(3000, function () {
        console.log('Server listening on port 3000');
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
