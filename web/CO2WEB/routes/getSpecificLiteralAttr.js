/**
 *  * Router
 *corresponds to agent: GetLiteralData(with name params)
 * TODO:: this most likely should be unified under getAttrList path
 * @type {log4js}
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

var express = require('express');
var router = express.Router();

var config = require("../config");

var LiteralData = require("../agents/GetLiteralData");

router.use(function(req, res, next) {
    res.header("Access-Control-Allow-Origin", "*");
    res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
    next();
});

router.post('/', function (req, res, next) {
    logger.debug("rounter getSpecName");
    if(!req.body) {
        next(new Error("Can not find req body"));
    }
    console.log(req.body);
    let parsedBody = JSON.parse(req.body);
    logger.debug(parsedBody);

    let uri = parsedBody.uri;

    let names = parsedBody.names;
    logger.debug("look for spec name: "+names+"Fron uri:"+uri);

    if(!uri || !names){
        logger.debug("Can not find uri or namelist")
        next(new Error("Can not find uri or namelist in req body"))
        return;
    }

    LiteralData(function (err, result) {
        if(err){
            next(err);
            return;
        }
        logger.debug(result);
        res.json(result); //render the view with this value

    },uri, {specificNames:names})
});

module.exports = router;