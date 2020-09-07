

var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

var express = require('express');
var cacheRouter  = require("../../agents/Cache");



var mapRouterFactory = function (router, getCoordinatesData, texts, view) {

    var textsOb   = {
        title:texts.title || "Map",
        subtitle:texts.subtitle || "Map",
        description:texts.description || ""
    }



    router.get('/', function(req, res, next) {
        res.render(view);//TODO: render with provided texts
        //send a file
    });

    router = cacheRouter(router).get('/coordinates', getCoordinatesData, { expiredTime:3600, sendResult});

    function sendResult(result, res) {
        console.log('***********************send result********************************');
        res.json(JSON.parse(result))
    }
    return router;
}



module.exports = mapRouterFactory;
