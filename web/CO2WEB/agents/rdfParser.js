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
    if (!opts.file || !opts.uri) {
        throw new Error("file and uri undefined");//TODO: CHECK ERR HANDLING END POINT
    }
    this.file = (typeof opts.file === "string"? opts.file:opts.file.toString());
    this.uri = opts.uri;
    this.mimeType = opts.mimeType || 'application/rdf+xml';


   this.parseBody();
};




RdfParser.RdfParser.prototype =  {
    parseBody: function() {

        try{
            $rdf.parse(this.file, this.store, this.uri, this.mimeType);// parse rdf
        }catch(err){
           throw err;
            console.log(err)
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

    },
    geoCoordsQuery : function (callback) {
        var qs = `
    PREFIX j.1: <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#>
        PREFIX j.2: <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#>
  
        select distinct ?cVname ?cName ?value
        where {
        ?cVname a j.1:CoordinateValue;
    j.2:isValueOf ?cName.

        ?cVname a j.1:CoordinateValue;
    j.2:numericalValue ?value.
}`;

        let coordiArr = {};
        this.mquery(qs, function (err, data) {
            if(err){
                callback(err);
                return;
            }
            //console.log(data)
            data.forEach(function (item) {
                let value = item['?value']['value'];
                let uri = item['?cName']['value']

                let name=getName(uri);
                coordiArr[name] = coordiArr[name]||{};
                coordiArr[name][isXorY(uri)] = value;
            })
            callback(null, coordiArr);
        })

        function isXorY(uri) {
            return uri.includes("#x_coordinate")?"x":"y";
        }
        function getName(uri) {
            return "http://www.jparksimulator.com/"+uri.split('_of_')[1]+".owl";
        }
    }


};

module.exports = RdfParser;