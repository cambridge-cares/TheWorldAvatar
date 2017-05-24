/**
 * Created by Shaocong on 4/4/2017.
 */
/***import modules**/
var $rdf = require('rdflib');
var fs = require('fs');


/***
 * opts = {
 *
 *fileUrl :
 * uri:
 * mimeType
 * }
 *
 *
 *****/

//TODO: check two things:
    //TODO: 1.HOW IS IT ASYN?
    //TODO: LAZY INITIATION


var rdfParser = function (opts) {

    var graph;




    /***define default value for options**/
    fileUrl = opts.fileUrl;
    uri = opts.uri;
      mimeType = opts.mimeType || 'application/rdf+xml';
        if (!fileUrl) {
            return new Error("file location undefined");//TODO: CHECK ERR HANDLING END POINT
        }
        if (!uri) {
            return new Error("owl url undefined");//TODO: CHECK ERR HANDLING END POINT
        }
    /**
     * utility function to parse the rdf file body
     * @param fileUrl
     * @param uri
     * @param mimeType
     * @param callback
     */
    function parseBody() {

        var body = fs.readFileSync(fileUrl, {encoding: 'utf8'});
//TODO: CHECK ERR HANDLING END POINT

        var store = $rdf.graph();//create empty graph object to be populated in


        try {
            $rdf.parse(body, store, uri, mimeType);// parse rdf

            return store;

        } catch (err) {

            return err;
        }

    };

        graph = parseBody();





        var  findNode = function(nodeUrl) {
            return $rdf.sym(nodeUrl);
        }


        var  findValue = function(nodeUrl, propertyUrl) {
            let node = findNode(nodeUrl);
            let property = findNode(propertyUrl);
             console.log(node);
             console.log(property);
            // console.log(graph);
            return (graph.any(node, property));
        };

    return {
        graph: graph,
            findNode: findNode,
        findValue: findValue


    };

};


/**export module**/
module.exports = rdfParser;
















