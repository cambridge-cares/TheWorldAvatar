/****
 * Factory to create a router with file-connection-reader module.
 * Parameter : topNode file
 * @type {*}
 */
var express = require('express');
var connectionsReader = require("../../agents/fileConnection.js");
var cacheRouter  = require("../../agents/Cache");

/* GET users listing. */

//TODO: buffer logic, so no need recalculate for each request, but still robust,
//TODO: could be time, or responding to change
/****
 * Factory to create a router with file-connection-reader module.
 * @param topNodeAddress  address of top node file
 */
var visualizationRouterFactory = function (topNodeAddress) {
    var router = express.Router();

    /**
     * This wrapper bc cache module requires all other params come after cb
     * @constructor
     */


    var getNormal = connectionsReader.getChildrenRecur.bind(null, {topnode : topNodeAddress})
    var getWImport = connectionsReader.getChildrenRecur.bind(null, {showImport : true, topnode : topNodeAddress})
    var getService = connectionsReader.getChildrenRecur.bind(null, { showServiceOnly : true, showServiceUrl: true,  topnode : topNodeAddress});

    function sendRender(results, res) {
        res.render('visual', { result: results }); //render the view with
    }


    const expiredTime = 36;

    router = cacheRouter(router).get('/', getNormal, { sendResult:sendRender,expiredTime});

    router = cacheRouter(router).get('/includeImport', getWImport, { expiredTime});

    router = cacheRouter(router).get('/showServiceOnly', getService, { expiredTime});



    return router;
};


module.exports = visualizationRouterFactory;
