/***
 * rdfParser
 */
/***import modules**/
let $rdf = require('rdflib');
let fs = require('fs');
let libxmljs = require("libxmljs");
let util = require('util');


/***
 * Sync function to parse rdf graph file in xml format.  throw errors
 * @param opts [fileUrl|mimeType]
 * @returns {{graph: *,  findValue: findValue }}
 * @ return function findValue
 */

let rdfParser = function (opts, callback) {

    let myUri;
    let store = $rdf.graph();//create empty graph object to be populated in


    /***define default value for options**/
    let fileUrl = opts.fileUrl;
    //TODO: get my uri from xml base defination
    let mimeType = opts.mimeType || 'application/rdf+xml';

    let webRetrieve = opts.webRetreive || false;//default from local
    if (!fileUrl) {
        throw new Error("file location undefined");//TODO: CHECK ERR HANDLING END POINT
    }




        /*define return functions**/
        let defineSym = function (nodeUrl) {
            return $rdf.sym(nodeUrl);
        };

        /**
         *
         * @param nodeUrl
         * @param propertyUrl
         * @returns [ {subject: {value,termType,datatype}, predate:{value,termType,datatype}, object:{value,termType,datatype}} ]
         */
        let search = function (nodeUrl, propertyUrl) {
            //test nodeUrl = "http://www.theworldavatar.com/JurongIsland.owl#CM_BiodieselPlant-3";
            //test propertyUrl = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/eco-industrialPark.owl#hasIRI";
            try {
                let node = defineSym(nodeUrl);

                let property = propertyUrl ? defineSym(propertyUrl) : undefined;
                console.log("node in search: " + nodeUrl);
                console.log("node in search: " + node);

                console.log("property: " + property);
              //  return store.statementsMatching(node, property, undefined);
                return (store.any(node, property));
            } catch(err){
                throw err;
            }
        };

        let add = function (s, p, o, oIsNode) {
            try{
                let o = oIsNode?defineSym(o):o;
                add(defineSym(s), defineSym(p), o);

            }catch(err){
                throw err;
            }

        };
        
        let query = function (queryStr, callback) {
            let dataset = [];
            store.query($rdf.SPARQLToQuery(queryStr, false, store), function (data) {//each data point

                dataset.push(data)
            }, null, function (err) {//when all is done
                if(err){
                    console.log(err);
                    callback(err);
                }
                callback(null, dataset);
            });
        };

   function getUri(root){

        let uri = root.find('//owl:Ontology', {owl:'http://www.w3.org/2002/07/owl#'});
        if(!uri || uri.length < 1){
            return null;
        }

        return uri[0].attr("about").value();

    };
        /**
         * utility function to parse the rdf file body, sync function, may throw err
         * @param fileUrl
         * @param uri
         * @param mimeType
         * @param callback
         */
        function parseBody(body) {

//TODO: CHECK ERR HANDLING END POINT

            try {
                /*---------retrieve base URI------------------*/
                let xmlDoc = libxmljs.parseXml(body);
                let root = xmlDoc.root();
                let myUri = getUri(root)

                //console.log(myUri);
               /*-------retrieve base URI END----------------*/

                $rdf.parse(body, store, myUri, mimeType);// parse rdf

            } catch (err) {

                throw err;
            }

        };


            let returnObj = {
                graph: store,
                search: search,
                add: add,
                query:query
            };


          if(!webRetrieve) {
              try {   /*-------read Local file----------------*/
                  let body = fs.readFileSync(fileUrl, {encoding: 'utf8'});
                  parseBody(body);
              return returnObj
              } catch (err) {
                  throw err;
              }
          } else{   /*-------Fetch from web----------------*/

              let timeout = 1000; // 5000 ms timeout
              let fetcher = new $rdf.Fetcher(store, timeout);
         console.log("now fetch")
              fetcher.nowOrWhenFetched(fileUrl, function(ok, body, xhr) {

                  if (!ok) {
                      console.log("Oops, something happened and couldn't fetch data");
                      callback(new Error("File can not be fetched"));
                  } else {
                      // do something with the data in the store (see below)

                    callback(null, returnObj);

                  }
              })
          }




};


/**export module**/
module.exports = rdfParser;
















