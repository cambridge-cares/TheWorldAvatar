/**
A wrapper on rdflib module to simplify the usage.
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

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

    /***
    parse the file
    ***/
    parseBody: function() {
        try{
            $rdf.parse(this.file, this.store, this.uri, this.mimeType);// parse rdf
        }catch(err){
            logger.debug(err)
        }

    },


    /***
    define a symbol
    ***/
    defineSym  : function (nodeUrl) {
        return $rdf.sym(nodeUrl);
    },
    /**
    add a triple
    ***/
    add : function (s, p, o, oIsNode) {
        try{
            let o = oIsNode?this.defineSym(o):o;
            this.store.add(this.defineSym(s), this.defineSym(p), o);//TODO: cehck this

        }catch(err){

            throw err;
        }

    },
    /***
    execute a sparql query
    ***/
    mquery : function (queryStr, callback) {
        let dataset = [];

        if(!this.store){
            callback(new Error("body not parsed"));
        }
      //  logger.debug(JSON.stringify(this.store))

        if(this.testUnion(queryStr)){
            this.fakeUnion(queryStr, callback);

        } else if(this.testFilterRegex(queryStr)){

            this.fakeFilterRegex(queryStr, callback);
        }


        this.store.query(new $rdf.SPARQLToQuery(queryStr, false, this.store), function (data) {//each data point


         //  logger.debug("@@@@@@@@@@@@@@@@@@@@")
            //logger.debug(data);
            dataset.push(data)
        }, null, function (err) {//when all is done
            if(err){
                logger.debug(err);
                callback(err);
            }

           // logger.debug("dataset: " )
           // logger.debug(dataset);
            callback(null, dataset);
        });


    },

    /***
    search triples with specific s and p
    @params:
    nodeUrl: s
    propertyUrl: p
    
    ***/
    search : function (nodeUrl, propertyUrl) {
        //test nodeUrl = "http://www.theworldavatar.com/JurongIsland.owl#CM_BiodieselPlant-3";
        //test propertyUrl = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/eco-industrialPark.owl#hasIRI";
        try {
            let node = this.defineSym(nodeUrl);

            let property = propertyUrl ? this.defineSym(propertyUrl) : undefined;
            logger.debug("node in search: " + nodeUrl);
            logger.debug("node in search: " + node);

            logger.debug("property: " + property);
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
        let self = this
        var qs = `
    PREFIX j.1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>
        PREFIX j.2: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
		PREFIX j.3: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#>
  
        select distinct ?cVname ?cName ?value
        where {
        ?cName a j.3:StraightCoordinate;
    j.2:hasValue ?cVname.

        ?cVname a j.1:CoordinateValue;
    j.2:numericalValue ?value.
}`;

        let coordiArr = {};
        this.mquery(qs, function (err, data) {
            if(err){
                callback(err);
                return;
            }
            console.log(data)
            data.forEach(function (item) {
                let value = item['?value']['value'];
                let name = item['?cName']['value']
              let uri = self.uri;
                coordiArr[uri] = coordiArr[uri]||{};
                coordiArr[uri][isXorY(name)] = value;
            })
            callback(null, coordiArr);
        })

        function isXorY(uri) {
            return uri.includes("#x_coordinate")?"x":"y";
        }
        function getName(uri) {
            return "http://www.jparksimulator.com/"+uri.split('_of_')[1]+".owl";
        }
    },

    testUnion : function(qstr){
        return qstr.toLowerCase().includes("union");
    },
    testFilterRegex: function(qstr){
      let lqstr  =  qstr.toLowerCase()
        return lqstr.includes("filter")&&lqstr.includes("regex");
    },
    /***
     * Because rdflib, for fuck's sake, only support three keywords in sparql, so
     * a pre-processor function to pre-process => send the queries => process result
     * Note: rdflib FILTER only supports regex and ><=
     *
     */
    fakeUnion : function (query) {
        //PREFIX dc10:  <http://purl.org/dc/elements/1.0/>
        //"SELECT ?title WHERE  { { ?book dc10:title  ?title } UNION { ?book dc11:title  ?title } }"
        //extract prefix, extract single select, extract single where
        query = query.toLowerCase();
        let prefixStr = query.split("where");
        if(!prefixStr){
            logger.error("no where defined in clause, can not parse");
        }
        prefixStr = prefixStr[0];
        logger.debug("prefix: ");
        logger.debug(prefixStr);
        var regex = /select +(\?[a-zA-Z]+) where/;
    },

    fakeFilterRegex : function (){


    },



};


/***
unwrap query results into an array of items
***/
RdfParser.unwrapResult = function (result, type) {
    if(!('head' in result) || !('results' in result) || !('bindings' in result['results']) || result['results']['bindings'].length === 0){
        console.log('wrong format')
        return null
    }
    let unwrapped = {}

   let varNames = result['head']['vars'];
    
  if(type!=='item'){

    for(let varname of varNames){
        console.log(result['head']['vars'])
        console.log(varname)
        if(!(result['head']['vars'].includes(varname))){
            return null
        }
    unwrapped[varname] = []
    }
} else {
    unwrapped = [];
}
      
    //unwrap
    console.log('upwrap')
    console.log(unwrapped)
    for(let line of result['results']['bindings']){
        let item = {};
        for(let varname of varNames){
let value = varname in line?  line[varname]['value']:'';
            if(type==='item')
            {
         item[varname] = value;
            }
            else
            {
        
         unwrapped[varname].push(value);
            }
        }
        if(type==='item'){
                     unwrapped.push(item);
        }
    }
    
    return unwrapped;//return unwrapped result
}
module.exports = RdfParser;