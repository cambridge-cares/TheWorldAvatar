"use strict";
/***
 
 没有结构性数据，考虑全程用流来做，如果有流的sparqlengine就直接套，不然就流local graph来query
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';
//import
var path = require('path');
var libxmljs = require("libxmljs");
var proj4 = require('proj4');


var async = require('async');
var readdirp = require('readdirp');
var fs = require('graceful-fs');
var util = require('util');
var config = require('../config.js');
//var folderLocation = config.root;
let request = require('request');
let stringtype = require('is-string')
let Parser = require('node-xml-stream')
const RdfParser = require('../agents/rdfParser');
const { Readable,PassThrough } = require('stream');

/**out a asyn function, provide data :
 [
 {source: , target: , level: }
 ]
 ****/
//commented out lazy innitiation
//var connections = [];
    //todo: add cluster logic
    
var owlProcessor = {
PREDICATE:['Eco-industrialPark:hasIRI','system:hasIRI' ],
queryStr:`
PREFIX Eco-industrialPark: <http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl#>
PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
    select distinct ?uri
    where {
    {?a Eco-industrialPark:hasIRI ?uri;}
    UNION {?a system:hasIRI ?uri;}
     @placeholder@
     }
`,
UnionImport:`
  UNION{?a system:hasIRI ?uri;}
`
};


//todo: add function to add extra query on the fly

/**
 * connect to an address, if find any link, pass to buffer
 * */
owlProcessor.doConnect = function(address, level) {
    const me = this
    me.linkCounter++;
    
    return new Promise((resolve, reject)=>{
        this.connectPromise(address, level).then(result=>{
            me.linkCounter--;
            console.log('linkcounter');
            console.log(me.linkCounter);
        
            if(result&&result.length>0){
                result.forEach(item=>{
                    if(level>=2) {
                        this.parentMap[item] = address in this.parentMap ? this.parentMap[address] : address;
                    }
                    me.result.push({'source':address, 'target':item, 'level':level})
                    //todo: cluster result on the fly
                })
                console.log(me.result.length)
            }
            if( me.linkCounter === 0){//end condition, return
                console.log('all end, print result')
                //end buffer
                if(me.buffer) {
                    me.buffer.push(null);
                } else {//directly resolve
                    resolve(me.result)
                }
                }
        
        })
            .catch(err=>{
                reject(err)
            })
    })
    

}

owlProcessor.doCluster = function (result) {
    //get all level 0
    
};

/***
 * async connect to an address and query it to get inter file links
 * @param address: local file, remote file or remote endpoint
 * @returns {Promise}
 */
owlProcessor.connectPromise = function (address, level) {
    //endpoint or file?
    const me = this
    return new Promise((resolve, reject)=>{
        if(!stringtype(address)){
            return new Error('address for this node is not a string')
        }
        let fileP = null;
        if (fs.existsSync(address)){//is local
            address = path.resolve(address);
            fileP = this.fileStreamPromise(address);
        } else{ //remote url
            fileP = this.urlPromise(address);
        }
        //stream parse fileP
        fileP.then(function (instream) {
            if(!instream){//test endpoint
                //todo: run endpoint
                console.log('check endpoint')
                me.queryPromise(address, 'endpoint',level).then(result =>{
                    console.log('ep result:');
                    console.log(result)
                   resolve(result)
                });
                
                
                
                
            } else {
                me.xmlstreamParser(instream, level).then(result => {
                  resolve(result)
                });
            }
        }).catch(function (err) {
            reject(err)
        });
        
    })
}

/***
 * test if a url contains a file
 * @param loc
 * @returns {Promise for a null if not a file, a file stream otherwise}
 */
owlProcessor.urlPromise = function (loc) {
    //send request
    return new Promise((resolve, reject) => {
            //check if file, if not, test if an endpoint
            //probably can still send for query if not sure if an endpoint...
            //quick xml parse, we don't wa
         
             owlProcessor.checkFile(loc).then((result)=>{
                 "use strict";
                 if(!result){
                     //return as endpoint
                     resolve(null)
                 }
                 resolve(result)
                 
             }).catch(err=>{
                 //todo: handle possible request error
                 console.log('err connect: '+loc);
                 
             })

    })
}

/**
 * stream xml parse a file to get inter uri links
 * predicates to extract defined in this.PREDICATE
 * @param instream: in stream for parsing
 * @returns {Promise for an array of raw links}
 */
owlProcessor.xmlstreamParser = function (instream, level) {
    const me = this;
    return new Promise((resolve, reject) => {
        const PREDICATE = this.PREDICATE;
        let parser = new Parser();
        let flagOuter =false, flag = false, result = [];
        parser.on('opentag', (name) => {
            if(me.OUTER && name === me.OUTER){
                flagOuter = true;
            }
            if((!me.OUTER || flagOuter) && PREDICATE.includes(name)){
                flag = true;
            }
        });
        parser.on('text', (text) => {
            if(flag){
                result.push(text);
                console.log(text)
                
                if(me.buffer) {me.buffer.push(text+'@'+(level))};
                flag = false
            }
        
        });
        
        parser.on('closetag', (name)=>{
            if(me.OUTER && name === me.OUTER){
                flagOuter = false;
            }
        })
        parser.on('error', err => {
            // Handle a parsing error
            console.log('err in parsing file')
            parser.end()
            //todo: not valid
            reject(err)
        });
        
        parser.on('finish', () => {
            console.log('finished parsing file')
            //todo: not valid
            parser.end()

            resolve(result)
        });
    
        instream.pipe(parser);
    
    })
    
};

owlProcessor.checkFile = function (loc) {
    return new Promise((resolve, reject) => {
        let parser = new Parser();
        const clone = new PassThrough();
    
        parser.on('opentag', (name, attrs) => {
        if(name === 'owl:Ontology'){
            console.log(loc+' pass validation');
            resolve(clone)
            parser.end()
        }
        
    });
    parser.on('error', err => {
        // Handle a parsing error
        console.log('err,we know now it is not valid')
        parser.end()
        resolve(null)
    });
    
    parser.on('finish', () => {
        console.log('finished checking for validation')
        resolve(null)
    });
    
        let res = request.get(loc, {timeout: 10000, agent: false}).on('error', function (err) {
            reject(err)//don't throw
        });
        res.pipe(parser)
        res.pipe(clone)
    
    })

};

/**
 * return a promise for async parse a sparql query
 * @param data
 * @param type
 * @returns {promise for query result}
 */
owlProcessor.queryPromise = function (loc, type, level) {
    console.log('query lvel: '+level)
    const self = this;
    switch(type){
        case 'endpoint':
            //construct query parameter
            console.log('query for end point')
            return new Promise((resolve, reject)=>{
                //run query against endpoint
                request.get(loc, {qs:{'query':self.queryStr,'output':'json'},timeout: 1500, agent: false}, function (err, res, body) {
                    console.log('endpoint resquest result')
                    if (err) reject(err);//don't throw
                    //unwrap query
                    console.log(body)
    
                    body = JSON.parse(body)
                    let {uri} = RdfParser.unwrapResult(body, ['uri']);
                    console.log(uri)
                    let tobuffer = uri.map((text)=> {return text+'@'+(level+1)}).join(';')
                    self.buffer.push(tobuffer);
                         resolve(uri)
                })
            });
            
            break;
            
        case 'local':
            //construct local graph then parse
            
            break;
            
        default:
            return new Error('Undefined query type')
    }
    

}


/**
 * return a promise for async read a locak file
 * @param loc
 * @returns {Promise for file}
 */
owlProcessor.filePromise = function (loc) {
    return new Promise(function (resolve,reject) {
        fs.readFile(loc, (err, data)=>{
            "use strict";
            err? reject(err):resolve(data);
        })
    })
    
}
/**
 * return a promise for async stream read a local file
 * @param loc
 * @returns {Promise for file stream}
 */
owlProcessor.fileStreamPromise = function (loc) {
    return new Promise(function (resolve,reject) {
        resolve(fs.createReadStream(loc))
    })
}

    /***
     * Utility function: Process href to be actual file uri
     * If href contains mark char:#, delete it
     * @param href, href string to be processed
     * @returns processed href string
     */

    owlProcessor.init = function (options) {
        this.loc = options.topnode;
        //modify query
        let extraQ = options.extraQuery?options.extraQuery:'';
        let extraP = options.extraPredicate?options.extraPredicate:'';
        let showImport = options.showImport || false; // if showServiceOnly is chosen, will not show Import
        if(showImport){
            owlProcessor.PREDICATE.push("owl:imports");
            extraQ = extraQ.query + self.UnionImport;
        }
        this.queryStr = this.queryStr.replace('@placeholder@', extraQ);
        if(extraP){owlProcessor.PREDICATE.push(extraP);}
        this.OUTER = options.outer?options.outer:null;
        console.log(this.queryStr)
    
        //process variable init
        this.processed = new Set();
        this.result = [];
        this. linkCounter = 0;
        this.parentMap = {}
    };

owlProcessor.packIntoClusterData = function (rawconn) {
    //get level 1 connections
    let self = this
    let firstl = [], subconnections = {};
    rawconn.forEach((link)=>{
        if(link.level === 1){
            firstl.push(link)
        } else{
            let parent = self.parentMap[link.target]
            if (parent in subconnections){
                subconnections[parent].connections.push(link)
            } else {
                subconnections[parent] = {connections:[]}
            }
        }
    })
    //pack into relative subconnctions
    return {connections:firstl, subconnections:subconnections}
}
/**
 * main funtion
 * @param options
 * @returns {Promise}
 */
owlProcessor.process = function (options) {
        //let loc ='http://www.theworldavatar.com/SemakauBuildingLayer.owl'
    /*init paramters*/
        let self = this
        this.init(options);
        this.doConnect(this.loc, 1);
        this.buffer = new Readable({read() {}});

      return new Promise((resolve, reject)=>{
          /*buffer logic**/
          this.buffer.on('data', (chunk) => {//receive new data:string of uris
              //process to get links
              console.log(chunk.toString())
              let uris = chunk.toString().split(';');
              for (let item of uris){
                  if(item){
                      let uri = item.split('@');
                      let level = uri[1];
                      uri = uri[0];
                      if(!self.processed.has(uri)){
                          self.processed.add(uri);
                          self.doConnect(uri, parseInt(level)+1);//if find new uri, do connect
                      }
                  }
              }
        
          });
    
          this.buffer.on('end', () => {
              console.log('end of search');
              /*END OF ALL RETRIEVING************/
              //console.log(this.parentMap)
              self.buffer = new Readable({read() {}});
              resolve(self.packIntoClusterData(self.result))
          });
          this.buffer.on('close', () => {
              console.log('end of search. should return');
          });
          
      })
    

        
      
    };



owlProcessor.uriList2DiskLoc = function (uriArr, diskroot) {
    diskroot = diskroot || config.root;
    return uriArr.map(function (item) {
        // logger.debug("map:"+item)
        let diskLoc = item.replace("http://www.theworldavatar.com",diskroot);
        diskLoc = diskLoc.replace("http://www.jparksimulator.com",diskroot);
        diskLoc = diskLoc.split('#')[0]
        return {uri:item, diskLoc:diskLoc}
    });
};
    

    
//test
//console.time('conn')
   // owlProcessor.doConnect(loc)
/**
var a = Object.create(owlProcessor)
    a.process( {topnode:"http://www.theworldavatar.com/SemakauBuildingLayer.owl"}).then((res)=>{
        console.log('print results')
        console.log((res))});
var b = Object.create(owlProcessor)
//b.process( {topnode:"http://www.theworldavatar.com/SemakauIsland.owl"}).then((res)=>{
  //  console.log('print')
    //console.log((res))});
**/
module.exports = owlProcessor;
