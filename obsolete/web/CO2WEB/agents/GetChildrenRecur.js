/**
  This module is unfinished, and is aimed to replace the current fileConnection module in a more elegant way.
 * Rewrite FileConnection function in endpooint query style
 * Provides all sorts of function for retrieve and process specific data
 */

var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';
//import
var path = require('path');
var libxmljs = require("libxmljs");
var proj4 = require('proj4');


var async = require('async');
var fs = require('graceful-fs');
var util = require('util');
var config = require('../config.js');
const parser = require('../agents/rdfParser');

//var folderLocation = config.root;


/*constructor*/
var GetChildrenRecur = function (options, callback) {
    this.mergeOptions(options);
    this.prefix = [];
    this.QStrMap = new Map();
    this.results = [];
    this.queryStrs = [];
    this.countAsChildren = [];
    logger.debug(this.QStrMap.toString());//debug out the constructed query map
    this.constructQueryStrs();
};

/*prototype**/
GetChildrenRecur.prototype = {
    mergeOptions: function (options) {
        /*settings***********************/
        const defaultQueryContent = ["children", "myUri"];
        //chidrena and myUi are must, do a merge function properly maybe
        this.queryContent = new Set(options.queryContent.concat(defaultQueryContent));
        //["children", "myUri", "geoCoord", "type","service","import"];
        this.topnodeLocation = options.topnode;
        //TODO: define which links which have recur run on them
    },

    constructPrefix: function (prefix) {
        let prefixStr = "";
        for (ns in prefix) {
            prefixStr += "PREFIX " + ns + ": <" + prefix[ns] + ">\n";
        }
        return prefixStr;
    },
    //According to options, set queryStrArr and result Attribute list
    constructQueryStrs: function () {
        let pos2Remove = [];
        for (let queryChoice of this.queryContent) {
            if(this.QStrMap.has(queryChoice)) {
                this.queryStrs.push(this.QStrMap.get(queryChoice));
            } else{
                pos2Remove.push(this.queryContent.indexOf(queryChoice));
            }
        };
        this.queryContent =  this.deleteInArray(this.queryContent, pos2Remove);
    },

    deleteInArray: function(arr, pos2Remove){
        pos2Remove = pos2Remove.sort(function compare(a,b) {
            return b - a;
        });

        pos2Remove.forEach(function (removepos) {
            arr.splice(removepos, 1);
        });

        return arr;
    },

    //TODO: rdflib does not support full sparql, have to make a work around for now
    setChildrenIPQueryStr: (function () {
        const prefix = {"industrialPark": "http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl#"};
        const q = `
   select distinct  ?children
    where {
    ?x industrialPark:hasIRI ?children.    
    }
`;
        this.qCon.set("childrenIP", this.constructPrefix(prefix)+ q);
    })(),

    setChildrenSysQueryStr: (function () {
        const prefix = {"system": "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#"};
        const q = `
   select distinct  ?children
    where {
    ?x system:hasIRI ?children.    
    }
`;
        this.qCon.set("childrenSys", this.constructPrefix(prefix)+ q);
    })(),

    setServiceUriQueryStr: (function () {
        const prefix = {"Service": "http://www.theworldavatar.com/Service.owl#"}
        const q = `
   select distinct  ?myUri
    where {
    ?x Service:hasUrl ?myUri.    
    }
`;
        this.qCon.set("serviceUri", this.constructPrefix(prefix)+ q);

    })(),
    //TODO:
    setGeoCoordQueryStr: (function () {


    })(),
    //TODO:
    setTypeQueryStr: (function () {

    })(),

    //TODO:
    setServiceQueryStr: (function () {

    })(),

    setImportQueryStr: (function () {
        const q = `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
   select distinct  ?import
    where {
         ?x a owl:Ontology.
         ?x owl:imports ?import.     
          }
    `;
        setCustomQueryStr("import", q);
    })(),
    //done
    setMyUriQueryStr: (function () {
      const q = `PREFIX owl: <http://www.w3.org/2002/07/owl#>
   select distinct  ?myUri 
    where {
    ?myUri a owl:Ontology.
    }`;
      this.setCustomQueryStr("myUri", q);//TODO: extract name from string instead of specify
    })(),

    setCustomQueryStr: function (attrName, queryStr) {
        if(typeof  queryStr === "object" && queryStr.hasOwnProperty(prefix) && queryStr.hasOwnProperty(q)){
            queryStr =  this.constructPrefix(queryStr.prefix)+ queryStr.q;
        }
        if(typeof queryStr !== "string"){
            logger.error("queryStr: " + queryStr +" is or can not be constructed into string");
            return;
        }
        this.queryContent.add(attrName);
        this.qCon.set(attrName, queryStr);
    },

    queryRecurStart: function (callback) {
        queryRecur(this.topnodeLocation, function (err) {
            if (err) {
                callback(err);
                return;
            }
            callback(null, this.resultArr2Obj(this.results));
        });
    },

    queryRecur: function (miuri, callback) {
        //query on mi self
        this.QueryMultiSameNodeFixStrs(miuri, function (err, miResultArr) {
            //okay map this to result Arr
            for (let idx = 0; idx < this.results.length; idx++) {
                this.results[idx].concat(miResultArr[idx]);
            }

            let childrenList = miResultArr[0]; //This is the byDefault children list
            //TODO: this criteria may change accordingly, what is an elegant way to do that I wonder
            if (childrenList.length === 0) {
                //A leaf node, go back
                callback();
                return;
            }
            async.map(childrenList, this.queryRecur, function (err, result) {


            });

        });
    },

    QueryMultiSameNodeFixStrs: function (uri, callback) {
        this.queryMultiSameNode(this.queryStrs, uri, callback);
    },

    /**
     * execute multi queries on same node, map the result
     * @param QueryStrs   array of queries
     * @param uri         uri of the node to query
     * @param callback callback(err, results array)
     */
    queryMultiSameNode: function (QueryStrs, uri, callback) {
        let nodeLoc = this.uri2DiskLoc(uri);
        fs.readFile(nodeLoc, function (errReadFile, file) {
            if (errReadFile) {
                logger.debug(errReadFile)
                callback(errReadFile);
                return;
            }
            async.map(QueryStrs, querySpecificNode, function (errAsync, results) {
                if (errAsync) {
                    callback(errAsync);
                    return;
                }
                callback(results);
            });

            function querySpecificNode(queryStr, callback) {
                this.querySingle(file, queryStr, function (err, singleResult) {
                    if (err) {
                        logger.debug(err)
                        callback(err);
                        return;
                    }
                    callback(null, singleResult);
                });
            }
        });
    },

    /** * Functions that send an individual query
     * TODO:In the future this could be sending to endpoint, now we do it locally in this program
     * @param uri
     * @param file
     * @param queryStr
     * @param callback
     */
    querySingle: function (uri, file, queryStr, callback) {
        if (uti.isBuffer(file) || typeof file === "string") {
            executeQuery(file);
        } else {
            let diskloc = this.uri2DiskLoc(uri);
            fs.readFile(diskloc, function (err, fileNewRead) {
                if (err) {
                    callback(err);
                    return;
                }
                executeQuery(fileNewRead);
            })
        }
        function executeQuery(file) {
            try {
                logger.debug("start rdf parsing")
                var mparser = new parser.RdfParser({uri: uri, file: file});
                logger.debug("end rdfparsing")

                mparser.mquery(queryStr, function (err, data) {//each data point
                    logger.debug("query")
                    if (err) {
                        logger.debug(err);
                        callback(err);
                    }
                    callback(null, data);
                });
            } catch (err) {
                throw err
            }
        }

    },

    /**
     * Convert a uri string or an uris array respectively to disklocations
     * @param uriArr  a string/ string array
     * @param diskroot, a specific root, default to root defined in config
     * @returns {*}
     */
    uri2DiskLoc: function (uriArr, diskroot) {
        diskroot = diskroot || config.root;
        if (variable.constructor === Array) {//great news, user input an array
            return uriArr.map(function (uri) {
                // logger.debug("map:"+item)
                return singleUri2DiskLoc(uri);
            });
        } else if (typeof  uriArr === "string") {
            return singleUri2DiskLoc(uri);
        } else {//other types? not possible
            logger.error("Wrong type uri, can not convert to disk loc")
            return null;
        }

        function singleUri2DiskLoc(uri) {
            let diskLoc = uri.replace("http://www.theworldavatar.com", diskroot);
            diskLoc = diskLoc.replace("http://www.jparksimulator.com", diskroot);
            return diskLoc;
        }
    },

    resultArr2Obj: function (resultArr) {
        let obj = {};
        if(resultArr.length !== this.queryContent.length){
            logger.error("Some fucking how attr list and result list does not have same length");
            throw new Error("Some fucking how attr list and result list does not have same length");
        }
        for(let idx = 0 ; idx < resultArr.length; idx++){
            let attrName = this.queryContent[idx];
            obj[attrName] = resultArr[idx];
        }
        return obj;
    }
};