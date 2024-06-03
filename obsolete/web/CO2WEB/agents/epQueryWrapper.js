//construct query parameter
var request = require('request');
var epQueryWrapper ={};
var async = require('async');
const csv = require('csv-parser');
const strStream = require('string-to-stream')
epQueryWrapper.singleQuery = function(loc){
    const self = this;

    let q = function(qStr, callback){
        request.get(loc, {qs:{'query':qStr,'output':'json'},timeout: 150000, agent: false, headers: [
                {
                    name: 'accept',
                    value: 'application/sparql-results+json'
                }
            ]}, function (err, res, body) {
            console.log('endpoint resquest result');

            if (err||!body||body.toLowerCase().includes('doctype')||body.includes('<?xml version="1.0" encoding="utf-8"?>')) {
                console.log('no result from endpoint, reject')
                callback(err);
                return;
            };//don't throw

            try {
                console.log(body);
                self.sparqlResultParser(body, function (err, result) {
                    if(err) callback(err);
                    callback(null, result);
                })
            }catch(e){
                callback(e)
            }

        })
    };
    return q;
}

epQueryWrapper.checkJson = function(body){
    try{
        JSON.parse(body)
    } catch(e){
        return false
    }
    return true
}

epQueryWrapper.unwrapJSON = function(result){
    if(!('head' in result) || !('results' in result) || !('bindings' in result['results']) || result['results']['bindings'].length === 0){
        throw Error('Endpoint returns with wrong format or required data can not be found in the database');
    }
    let names = result['head']['vars'];
    let lines = result['results']['bindings'];
    let processed = [];
    for (let line of lines){
        let lineObj ={};
        for (let name of names){
            lineObj[name] = line[name]['value'];
        }
        processed.push(lineObj)
    }
    return processed;
}

epQueryWrapper.sparqlResultParser = function(body,callback){
    let self =this;
    if (self.checkJson(body)){
        body = JSON.parse(body);
        //todo: rewrite unwrap
     try {
         let items = self.unwrapJSON(body);
         if(!items){
             callback(new Error('empty query result'));
             return;
         }
         callback(null, items)
     }catch(err){callback(err);}
    } else{//=> csv
        const results= [];
        strStream(body).pipe(csv()).on('data', (data)=>results.push(data))
            .on('end',()=>{
                "use strict";
                console.log('csv');
                console.log(results);
                callback(null, results);
            })
    }
}
epQueryWrapper.queryPromise = function (queryStr, endpoint){
console.log('query for end point')
return new Promise((resolve, reject)=>{
    //run query against endpoint
    if(typeof queryStr === 'string'){
        queryStr= [queryStr];
    }
    async.map(queryStr, epQueryWrapper.singleQuery(endpoint), function(err, resultArr){

        if(err){
            console.log(err)
            reject(err);
            return;
        }
        console.log('lenght of result'+resultArr.length);
        console.log(resultArr);

        for(let idx = 0; idx< resultArr.length; idx++){
            if(!resultArr[idx]){
                continue;
            }
            for(let item of resultArr[idx]){
                if(!'level'  in item){
                    item['level'] = idx+1;
                    console.log(item['level']);
                }
            }
        }

        console.log('packed resulut:');
        console.log([].concat.apply([], resultArr));
        resolve([].concat.apply([], resultArr));
    } )
});
}

module.exports = epQueryWrapper;