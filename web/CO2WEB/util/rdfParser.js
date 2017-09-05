/**
 * Created by Shaocong on 8/31/2017.
 */
let $rdf = require('rdflib');
let fs = require('fs');
let libxmljs = require("libxmljs");
let util = require('util');

var RdfParser ={};
RdfParser.RdfParser = function (opts) {
    this.store = $rdf.graph();
    this.fileUrl = opts.fileUrl;
    this.mimeType = opts.mimeType || 'application/rdf+xml';
    if (!this.fileUrl) {
        throw new Error("file location undefined");//TODO: CHECK ERR HANDLING END POINT
    }

    this.parseBody();
};




RdfParser.RdfParser.prototype =  {
    parseBody: function() {
        let fileUrl  = {};
        try {
            /*---------retrieve base URI------------------*/
            fileUrl = this.fileUrl;
            let body = fs.readFileSync(this.fileUrl, {encoding: 'utf8'});
            let xmlDoc = libxmljs.parseXml(body);
            let root = xmlDoc.root();
            myUri = this.myUri = this.getUri(root)||((root.attrs() && root.attrs().length > 0) ? root.attrs()[0].value() : null);//take ontology object defined to be uri

                if (!this.myUri) {//Also not found as ns definiton?
                    throw err(new Error("base IRI of owl file not defined in owl file:" + fileUrl));
                }

            /*-------retrieve base URI END----------------*/
            $rdf.parse(body, this.store, this.myUri, this.mimeType);// parse rdf

        } catch (err) {

            if(err.errno== -4058){
                console.log("no such file or directory: "+fileUrl)
               // console.log(err.message);

            } else{
                console.log("parse rdf error: " + fileUrl );
                throw err;
            }

        }
    },

    defineSym  : function (nodeUrl) {
        return $rdf.sym(nodeUrl);
    },
    add : function (s, p, o, oIsNode) {
        try{
            let o = oIsNode?this.defineSym(o):o;
            this.store.add(this.defineSym(s), this.defineSym(p), o);//TODO: cehck this

        }catch(err){

            throw err;
        }

    },
    mquery : function (queryStr, callback) {
        let dataset = [];

        //console.log(this.fileUrl)
        //console.log("!!!!!!!!!!!!!!!!!!!!!!!")

        //console.log("now query")
        //console.log(util.inspect(this));
        this.store.query($rdf.SPARQLToQuery(queryStr, false, this.store), function (data) {//each data point

          //  console.log("@@@@@@@@@@@@@@@@@@@@")
           // console.log(data);
            dataset.push(data)
        }, null, function (err) {//when all is done
            if(err){
                console.log(err);
                callback(err);
            }

           // console.log("dataset: " )
           // console.log(dataset);
            callback(null, dataset);
        });


    },
    search : function (nodeUrl, propertyUrl) {
        //test nodeUrl = "http://www.theworldavatar.com/JurongIsland.owl#CM_BiodieselPlant-3";
        //test propertyUrl = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/eco-industrialPark.owl#hasIRI";
        try {
            let node = this.defineSym(nodeUrl);

            let property = propertyUrl ? this.defineSym(propertyUrl) : undefined;
            console.log("node in search: " + nodeUrl);
            console.log("node in search: " + node);

            console.log("property: " + property);
            //  return store.statementsMatching(node, property, undefined);
            return (this.store.any(node, property));
        } catch(err){
            throw err;
        }
    },
    getUri : function(root){

        let uri = root.find('//owl:Ontology', {owl:'http://www.w3.org/2002/07/owl#'});
        if(!uri || uri.length < 1){
            return null;
        }

        return uri[0].attr("about").value();

    }


};

module.exports = RdfParser;