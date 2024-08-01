/**
 * This is a cached version of getSpecificLiteralAttr
 * @type {log4js}
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

var express = require('express');
var router = express.Router();

var config = require("../config");
var cacheRouter  = require("../agents/Cache");

var LiteralData = require("../agents/GetLiteralData");

router.use(function(req, res, next) {
    res.header("Access-Control-Allow-Origin", "*");
    res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
    next();
});

function req2args(req) {
    logger.debug("rounter getSpecName");
    if(!req.body) {
        return new Error("Can not find req body");
    }
    console.log(req.body);
    let parsedBody = JSON.parse(req.body);
    logger.debug(parsedBody);

    let uri = parsedBody.uri;
    let names = parsedBody.names;
    logger.debug("look for spec name: "+names+"Fron uri:"+uri);

    if(!uri || !names){
        logger.debug("Can not find uri or namelist")
       return new Error("Can not find uri or namelist in req body")
    }

    return [uri,{specificNames:names}]
}

function sendResult(result, res) {
    res.json(result); //render the view with this value

}
router = cacheRouter(router).post('/', LiteralData, {req2args, sendResult});


module.exports = router;