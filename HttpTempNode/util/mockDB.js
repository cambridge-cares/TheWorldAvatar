/**
 * Created by Shaocong on 4/20/2017.
 * Interface to be adopted by inner node owl/db data search
 * A json file is used to mock database
 */
let path = require('path');
let fs = require('fs');
$rdf = require('rdflib');
/***mock db json format
 * {"data" : [{"uri" : a, "value" : b}]}
 *
 *
 *
 */
let dbRoot =  "./ROOT/db.json";
/**
 * TODO:Interface function , to be implemented
 * search in db with an array of uris, return an array of found result object{uri:value}, and an array of all uris not found any results
 * @param uris2Search    array of uris to be searched in the db
 * @param callback      (err, results, notFoundSearchUris)    (err object, object array of Results  [{uri:value},...] , array of not found uris)
 */
function searchDB (uris2Search, callback){
    //TODO:callback(err, results, urisNotFound)
    //TODO: results array format [ {uriA : DataA}, {uriB : DataB}...  ]

    fs.readFile(dbRoot, function (err, body) {//read json file as mock db
        if(err){
            callback(err);
            return;
        }

        console.log("start searching db");
        let results = [];
        let notFound = [];
        let db = JSON.parse(body);
        for(let uri of uris2Search){
            let found = false;
            for(let idxItem = 0; idxItem < db.data.length&&!found; idxItem++){
                  if(db.data[idxItem].uri === uri){
                      found = true;
                      let temp= {};
                      temp[uri]  = db.data[idxItem].value;
                      results.push(temp);
                  } else if(idxItem ===db.data.length -1 &&!found){
                      notFound.push(uri);
                  }
            }
    }








    if(notFound.length < 1){
        console.log("all is found");

        callback (null, results);
    } else {
        console.log("found :"+results.length+", unfound:"+notFound.length);

        callback(null, results, notFound);
    }
    })


}

/****
 * TODO: interface function to be implemented
 * Get all child nodes defined in this owl db
 * @param options
 * @param callback
 */
function getChildren(options, callback){

    fs.readFile(dbRoot, function (err, body) {
        if(err){

            callback(err);
            return;
        }

        let db = JSON.parse(body);

        callback(null, db.children);
    })
}

/**
 * TODO: interface function to be implemented
 * Update data in OWL
 */
function update(){

}


module.exports = {searchDB, getChildren, update};