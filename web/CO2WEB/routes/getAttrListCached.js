/**
 * A cached version(has cache middleware in router) of getAttrList
 */

var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

var express = require('express');
var router = express.Router();

var config = require("../config");

var LiteralData = require("../agents/GetLiteralData");
var cacheRouter  = require("../agents/Cache");

router.use(function(req, res, next) {
    res.header("Access-Control-Allow-Origin", "*");
    res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
    next();
});

router = cacheRouter(router).post('/', LiteralData, {req2args, expiredTime:36});

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


module.exports = router;