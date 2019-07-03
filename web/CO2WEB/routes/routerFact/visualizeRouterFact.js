/****
 * Factory to create a router with file-connection-reader module.
 * Parameter : topNode file
 * @type {*}
 */
const express = require('express')
const owlProcesser = require('../../agents/fileConnection2Way.js')

const connectionsReader = Object.create(owlProcesser)
/* GET users listing. */

//TODO: buffer logic, so no need recalculate for each request, but still robust,
//TODO: could be time, or responding to change
/****
 * Factory to create a router with file-connection-reader module.
 * @param opts - topNodeAddress  address of top node file
 */
const visualizationRouterFactory = function (opts) {
  let router = express.Router()
  let viewName = opts.viewName ? opts.viewName : 'visual'

  router.get('/', function (req, res) {
    res.render(viewName) //render the view with this value, { result: JSON.stringify(results)
  })

  router.get('/links', function (req, res) {

    connectionsReader.process(opts).then((results) => {

      console.log('read connections')

      console.log(results)

      results.topnode = opts.topnode

      res.json(results) //render the view with this value

    })

  })

  router.get('/includeImport', function (req, res) {

    opts['showImport'] = true

    connectionsReader.process(opts).then((results) => {

            
            console.log("read connections");
            
            //res.setHeader('Content-Type', 'application/json');
            //res.json(results);//for testing
            //console.log(results)
            conns = results;
            results.topnode = opts.topnode;

            //res.setHeader('Content-Type', 'application/json');
            // res.json(results);
            res.json(results); //render the view with this value
        opts['showImport'] = false;


        });
    });

/**
    router.get('/showServiceOnly', function(req, res, next) {

        connectionsReader.getChildrenRecur({ showServiceOnly : true, showServiceUrl: true, topnode : topNodeAddress}, function (err, results) {

  return router
}

            console.log("read connections");

            //res.setHeader('Content-Type', 'application/json');
            // res.json(results);
            res.json(results); //render the view with this value


        });


    });
***/
    return router;
};


module.exports = visualizationRouterFactory;
