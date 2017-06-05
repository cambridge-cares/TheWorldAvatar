/**
 * Created by Shaocong on 4/20/2017.
 * Interface to be adopted by inner node owl/db data search
 * A json file is used to mock database
 */
/*import npm modules***/
let path = require('path');
let fs = require('fs');
let util = require('util');
/*import self modules***/
let rdfParser = require(__dirname+"/rdfParser.js");

/*file globals**/
let dbRoot = "./ROOT";
//TODO: check lazy initiation

try{
    var parser = (function initDB(){//lazy initiation
        /*read db owl file******/
        fs.readdir(dbRoot, function (err, files) {//read db owl file
            if(err){
                callback(err);
                return;
            }

            if(files.length < 1 ){
                callback(new Error("No owl file in root directory"));
                return;
            }

            /*search db******/
            try {
                let opts = {//format opts to feed in rdfParser
                    fileUrl : path.join(dbRoot,files[0])
                };
                return     rdfParser(opts);

            } catch(err){
                throw  err;
            }

        })
    })();

}catch(err){

    console.log(err);
}


/**
 * @param nodeIRI    nodeIRI to be searched
 * @param propertyIRI     if not defined, all triples  relative to nodeIRI will be returned
 * @param callback      (err, result)    (err object, object array Result
 */
function searchDB (nodeIRI, propertyIRI, callback){

    /*check params******/
    if(!nodeIRI){
        callback(new Error("nodeIRI not defined when searching db"));
        return;
    }

        /*search db******/


            propertyIRI = propertyIRI || undefined;
            console.log("search for node"+nodeIRI);
            let results = parser.search(nodeIRI, propertyIRI);//search in parsed graph for this property of this note
            //console.log("results"+util.inspect(results));

            callback(null, results);

}


/**
 * TODO: interface function to be implemented
 * Update data in OWL
 */
function update(nodeIRI, propertyIRI, newVal, callback){

    /*check params******/
    if(!nodeIRI || !propertyIRI){
        callback(new Error("nodeIRI or property not defined when searching db"));
        return;
    }

    console.log("update node: " + nodeIRI);
//delete old one. add new one = =



}


/***
 *
 * @param s
 * @param p
 * @param o
 * @param callback
 */
function add(s, p ,o,callback){


}


module.exports = {searchDB, update};