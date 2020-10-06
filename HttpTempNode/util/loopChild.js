/**
 * Created by Shaocong on 4/18/2017.
 * Loop same request for each child
 */


let path = require('path');
let async = require('async');
let request = require('request');
//let getChildren = require('./fileConnection');

let db = require('./mockDB');
let owlPath = path.join(__dirname, "ROOT");

function loopRequest(queryPath, queryParam, callback) {

    let options = {
        depth: 0,
        folderLocation: owlPath,
        showImport: false
    };

    //get all owl children => request to each, concat data results
    db.getChildren(options, function (err, children) {

        async.concat(children,
            function (childIp, callbackIndi) {//run async requests for each child in children
                let tempJson = {uris: [queryParam]};
                request(
                    {
                        method: 'GET',
                        uri: childIp + queryPath,
                        qs: {uris: JSON.stringify(tempJson)}

                    }
                    , function (err, response, body) {
                        if (err) {
                            callbackIndi(err);
                            return;
                        }
                        console.log("response body:" + body);
                        callbackIndi(null, JSON.parse(body).results);//TODO: body need parse? should callback with json
                    })


            }, function (err, results) {//results is now accumulated results of all requests
                if (err) {
                    callback(err);
                }
                console.log(results);
                callback(null, results);

            });


    });


}



module.exports = loopRequest;