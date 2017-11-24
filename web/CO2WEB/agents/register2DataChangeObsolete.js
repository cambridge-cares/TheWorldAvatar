/**
 * This is a obsolete module that have been replaced by the new changewatcher.
 * The module registeres for changes happen in other physically separated nodes
Register/deregister to subscribe to a data changing node, basically is a request with specific data format:
 {
 url:  string, my url+path,
 getInitData: boolean, if initData is returned
  register: boolean
  deregister: boolean
 }

 */
//TODO: how to store infor as who to register to ? => url link


var request = require('request')


exports.register = function(reqUrl, myUrl, callback) {
    request.post({url: reqUrl ,body: JSON.stringify({url:myUrl, getInitData:true, register:true}),headers: {
        'content-type': 'application/json'
    }, agent:false}, function (err,res ,body) {

        console.log("posting to dataObserve localhost 2000");

        if(err){
            console.log(err);
            callback(err);
            return;
        }
        console.log("intial body");
        console.log(body);
        callback(null, body);
    });

};


exports.deregister = function(reqUrl, myUrl, callback) {
    request.post({url: reqUrl ,body: JSON.stringify({url:myUrl, getInitData:true,deregister:true}),headers: {
        'content-type': 'application/json'
    }, agent:false}, function (err,res ,body) {

        console.log("posting to dataObserve localhost 2000");

        if(err){
            console.log(err);
            callback(err);
            return;
        }
        callback(null, body);

    });


}