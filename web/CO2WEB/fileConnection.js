/**
 * Created by Shaocong on 4/6/2017.
 */
//import
var path = require('path');
var libxmljs = require("libxmljs");
var async = require('async');
var readdirp = require('readdirp');
var fs = require('fs');
var util = require('util');
var config = require('./config.js');
var folderLocation = config.root;
let request = require('request');
let rootNode  = config.rootNode;
/**out a asyn function, provide data :
 [
 {source: , target:  }
 ]


 ****/

//var connections = [];

function readConnections(options, callback) {

    let depth = options.depth || 0;
    let showImport = options.showImport || false;
    // if (connections && connections.length > 0) {
    //      callback(null, connections);
    // } else {



    startFromRoot2GetConns(function (err, connections) {

        if (err) {
            callback(err);
            return;
        }
        callback(null, connections);

    });


    //  }


    function startFromRoot2GetConns(callback) {

        //read root file

        fs.readFile(path.join(folderLocation, rootNode), function (err, file) {
            if (err) {
                console.log("errReadingFile");
                callback(err);
                return;
            }

            loopChildrenRecur(file, 0, callback);

        })

    }

    function getXMLImports(root) {
        try {

            let targets = [];
            for (var ns of root.namespaces()) {
                //console.log(ns.prefix()+ ":"+ns.href());
                //get base from xml
                if (ns.prefix() !== null) {//if prefix not null [ self namespace is defined with null prefix]
                    console.log("prefix:" + ns.prefix());

                    var href = processHref(ns.href()); // text process href to extract #
                    targets.push(href.trim());// add to target list
                }
            }

            return targets;
        }
        catch (err) {
            throw  err;
        }
    }


    function getChildren(root) {

        var children = [];
        var namespaceOb = {};//construct namespaceOb for find in root with nested namespace

        namespaceOb['owl'] = "http://www.w3.org/2002/07/owl#";
        namespaceOb['Eco-industrialPark'] = "http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl#";
        namespaceOb['system'] = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#";

        console.log(util.inspect(namespaceOb));

        //find all node with hasIRI property
        var uris = root.find("//owl:NamedIndividual/Eco-industrialPark:hasIRI", namespaceOb);
        console.log("found node :" + uris.length);
        for (let curi of uris) {
            console.log(curi.name());
            children.push(curi.text().trim());//push to targets list
        }
        //find all node with hasIRI property
        let urisS = root.find("//owl:NamedIndividual/system:hasIRI", namespaceOb);
        console.log("found node :"+urisS.length);
        for(let curi of urisS){
            //    console.log(curi.name());
            children.push(curi.text().trim());//push to targets list
        }
        return children;
    }


       function loopChildrenRecur(file, level, callback) {

           console.log("------------loopChildRecur--------------------");

           try {


               var xmlDoc = libxmljs.parseXml(file);
               var root = xmlDoc.root();
               var myUri = (root.attrs() && root.attrs().length > 0) ? root.attrs()[0].value() : null;

               console.log("myURI" + myUri);
               //TODO: request for remote  VS search with name on current?
               //TODO: xml parse the file => get targets list == childList => request on each child file

               let children = getChildren(root);

               if (children.length < 1) {

                   let results = [];
                   console.log(myUri + " is a leaf node return");
                   if (showImport) {
                       try {
                           var imports = getXMLImports(root);
                           for (let imported of imports) {
                               results.push({source: myUri, target: imported})
                           }
                           ;

                       } catch (err) {
                           callback(err);
                           return;
                       }
                   }
                   callback(null, results);
                   return;
               }//else

               //get my links
               let myLinks = [];
               for (let target of children) {
                   console.log("child iri: " + target);
                   myLinks.push({source: myUri, target: target, level:level});
               }
               ;

               //request on each child, execute loopChild on it, then concate the result
               async.concat(children, requestChild, function (err, results) {
                   if (err) {
                       console.log("concat err");
                       console.log(err.message);
                       callback(err);
                       return;
                   }
                   console.log("concat results");

                   results = results.clean(null);
                   callback(null, results.concat(myLinks));

               });
           } catch (err) {
               callback(err);
           }


           function requestChild(iri, callback) {
               request.get(iri, {timeout: 100},function (err, response, body) {

                   if (err && err.code === 'ETIMEDOUT') {
                       console.log(err.code);
                       callback(null, null);
                       return;

                   }

                   if(err || response.statusCode !== 200){
                       console.log(err);
                       console.log("can not connect to "+ iri);
                       callback(null, null);
                       return;
                   }
                   if (response.statusCode === 200) {
                       console.log("req: " + iri);

                       loopChildrenRecur(body, level+1, callback);
                       return;
                   }

                   console.log("why the fuck did anyone get to this point");
                   callback(null, null);
               });

           }
    }


    Array.prototype.clean = function(deleteValue) {
        for (var i = 0; i < this.length; i++) {
            if (this[i] == deleteValue) {
                this.splice(i, 1);
                i--;
            }
        }
        return this;
    };
    /***
     *@deprecated
     * Walk through folder to get all connections
     *  * @param depth  : depth of reading into sub folders

     * @param callback
     */
    /**
     function walkThroughFolder2GetConns( callback) {

        //asyncly
//read dir of each file
//then xml extract the header, store this
        var fileList = [];
        readdirp({root: folderLocation, fileFilter: ['*.owl'], depth : depth})//only read one level, might be different for future with nested folder
            .on('data', function (entry) {
                console.log(entry.path);

                fileList.push(entry.path);

            })
            .on('end', function () {

                var fullPathList = [];

                for(var repath of fileList){
                    fullPathList.push(path.join(folderLocation, repath));
                }


                async.map(fullPathList, readAsync, function (err, results) {//read all files, read connections async
                    // results is now an array of stats for each file
                    if (err) {
                        callback(err);
                    }
                    for (let i = 0; i < results.length; i++) {
                        readConnectionsFromOwl(results[i], fileList[i]);
                    }

                    callback();
                });
            });

        function readAsync(file, callbackRead) {
            fs.readFile(file, 'utf8', callbackRead);

        }



    }


     /***
     *     @deprecated
     * Extract all connections in {source : ,target:} from a single owl file
     * @param file   : filebody
     * @param filepath
     */
    /**
     function readConnectionsFromOwl(file, filepath, callback) {
        var xmlDoc = libxmljs.parseXml(file);
        var root = xmlDoc.root();
        //my uri???
        var myUri;
        var targets = [];
        var children = [];
        myUri = (root.attrs()&&root.attrs().length > 0)?root.attrs()[0].value() : null;
        //console.log("base: " + myUri);

        //loop through each node, note down value of tap : hasURI

        var namespaceOb = {};//construct namespaceOb for find in root with nested namespace
        namespaceOb['owl'] = "http://www.w3.org/2002/07/owl#";
        namespaceOb['Eco-industrialPark'] = "http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl#";
        console.log(util.inspect(namespaceOb));

        //find all node with hasIRI property
        var uris = root.find("//owl:NamedIndividual/Eco-industrialPark:hasIRI", namespaceOb);
        console.log("found node :"+uris.length);
        for(var curi of uris){
            console.log(curi.name());
            targets.push(curi.text().trim());//push to targets list
        }

        //determine source url
        if (!myUri) {//base not defined in owl, then deduce from file path
            myUri = path.join(domain, filepath);
        }


        //loop through namespaces defined in root

        if(showImport) {
            for (var ns of root.namespaces()) {
                //console.log(ns.prefix()+ ":"+ns.href());
                //get base from xml
                if (ns.prefix() !== null) {//if prefix not null [ self namespace is defined with null prefix]
                    console.log("prefix:" + ns.prefix());

                    var href = processHref(ns.href()); // text process href to extract #
                    targets.push(href.trim());// add to target list
                }
            }
        }

        readConnectionInChildren(targets, function (err, results) {
            if(err){
                console.log(err);
            }

        });
        //pack into json
        for (let target of targets) {
            connections.push({source: myUri, target: target});
        }
    }
     }

     /**
     *     @deprecated

     * @param iriList
     * @param callback
     */
    /**
     function readConnectionInChildren(iriList, callback){

    async.concat(iriList, readConnectionInSingleChild, callback);


}
     **/
    /**
     *     @deprecated
     * @param iri
     * @param callback
     */
    /**
     function readConnectionInSingleChild(iri, callback){

    rdfParser({fileUrl:iri, webRetrieve :true}, function (err, mParser) {

        if(err){
            console.log(err);
            callback(null, null);//return no err message, so async won't terminate
        } else {
            let node = "http://www.w3.org/2002/07/owl#NamedIndividual";
            let property = "http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl#hasIRI";//property
            try {
                let resultObjArr = mParser.search(node, property);

                let results = [];
                for(let resultObj of resultObjArr){
                    results.push(resultObj.subject.value);
                }

                callback(null, results);


            }catch(err){
                console.log(err);
                callback(null, null);
            }
            }

    });

}
     **/

    /***
     * Utility function: Process href to be actual file uri
     * If href contains mark char:#, delete it
     * @param href, href string to be processed
     * @returns processed href string
     */
    function processHref(href) {

        if (href.endsWith("#")) {//TODO: other situations , like end with /?(also need actual prefix used to form a complete url)
            href = href.slice(0, -1);
        }
        return href;
    }
}

module.exports = readConnections;
