/**
 *  * Router
 *corresponds to agent: GetChildrenSingleFile
 * @type {log4js}
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

var express = require('express');
var router = express.Router();

var config = require("../config");

var getChildrenSingleFile = require("../agents/GetChildrenSingleFile");

router.use(function(req, res, next) {
  res.header("Access-Control-Allow-Origin", "*");
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
  next();
});

router.post('/', function (req, res, next) {
    if(!req.body) {
        next(new Error("Can not find req body"));
    }
    console.log(req.body);
    let parsedBody = JSON.parse(req.body);
    console.log(parsedBody);

    let uri = parsedBody.uri;

    if(!uri){
        console.log("Can not find uri")
        next(new Error("Can not find uri in req body"))
        return;
    }
    getChildrenSingleFile(uri, function (err, result) {
        if(err){
            next(err);
            return;
        }
        res.json(result); //render the view with this value

    })
});

module.exports = router;