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
PREDICATE:['Eco-industrialPark:hasIRI','system:hasIRI','system:hasSubsystem','j.0:hasSubsystem'],
queryStr:`
PREFIX Eco-industrialPark: <http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#>
PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
             PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
             PREFIX ontochem: 
<https://como.cheng.cam.ac.uk/kb/ontochem.owl#>
    select ?parent  ?uri
    where {
	{?parent  system:hasSubsystem ?uri;}    
    UNION {?parent  system:hasIRI ?uri;}
	UNION {?parent  Eco-industrialPark:hasIRI ?uri;}
	UNION {?uri rdf:type ontochem:ReactionMechanism .}
     @placeholder@
     }
`,
queryStr2:`
             PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
             PREFIX ontochem: 
<https://como.cheng.cam.ac.uk/kb/ontochem.owl#>
             SELECT ?uri
             WHERE {
                    { ?uri rdf:type ontochem:ReactionMechanism .}
                         @placeholder@
             }

`,
UnionImport:`
  UNION{?a owl:imports ?uri;}
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
        	console.log('tried connecting: '+address)
            address = me.diskLoc2Uri(address);//resolve address inconsistency
            address = me.equalHost(address);
            if(result&&result.length>0){
				let iset = new Set();
				console.log('found result: '+JSON.stringify(result));
                result.forEach(item=>{
                    let parent = null,label = '';
                    if(typeof item === 'object'){
                        if('uri' in item ){
                        parent = item['parent']?item['parent']:address;//if parent iri is null, just use address
                        label = item['label']?item['label']:'';
                        level = item['level']?item['level']:level;
                        item = item['uri'];

                        }
                    } else {
                        parent = address;
                          label = item;
                    }

                    if(!item){
                        console.log('Empty Item');
                        return;//skip null 
                    }

                    item = me.normalAddr(item);
                    item = me.equalHost(item);
                    console.log(item)
                       
					if(!iset.has(item)){
                        console.log('add')
						iset.add(item);
                    if(level>=2 && !(item in me.parentMap) && (parent!==item)) {
                        me.parentMap[item] = parent in me.parentMap ? me.parentMap[parent] : parent;
                    }
                    me.result.push({'source':parent, 'target':item, 'label': label ,'level':level});
                    console.log({'source':parent, 'target':item, 'label': label ,'level':level})
                    
                    
                    }
                    //todo: cluster result on the fly
                })
            }
            if( me.linkCounter <= 0){//end condition, return
                console.log('all end, print result')
				
                //end buffer
                if(me.buffer) {
                    me.buffer.push(null);
					me.buffer.pause();
                } else {//directly resolve
				console.log('request for one layer result')
				console.log(me.result);
                    resolve(me.result)
                }
                }
        
        }).catch(err=>{
				me.linkCounter--;
                console.log('err connect: '+err)
				            if( me.linkCounter <= 0){//end condition, return
                console.log('all end, print result')
                //end buffer
                if(me.buffer) {
                    me.buffer.push(null);
					me.buffer.pause();
                } else {//directly resolve
								console.log('request for one layer result')

                    resolve(me.result)
                }
                }
                //reject(err)
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
		    console.log('read local')
            address = path.resolve(address);
            fileP = this.fileStreamPromise(address);
        } else{ //remote url
            fileP = this.urlPromise(address);
        }
        //stream parse fileP
        fileP.then(function (instream) {
            if(!instream){//query endpoint
                //todo: run endpoint
                console.log('check endpoint')
                me.queryPromise(address, 'endpoint',level).then(result =>{
                    console.log('ep result connect p:');
                    console.log(result)
                    //!!!!!return result
                   resolve(result)
                }).catch((err)=>{
					reject(err)
				});

            } else {//query the file
                me.xmlstreamParser(instream, level).then(result => {
                    //!!!!!return result
                  resolve(result)
                }).catch((err)=>{
					reject(err)
				});
            }
        }).catch(function (err) {
			console.log(err)
            reject(err)
        });
        
    })
}


owlProcessor.normalAddr = function(loc){
	if(loc.charAt(loc.length-1)==='\/'){
        loc = loc.slice(0,loc.length-1);
    }
	let locs = loc.split('#');
	return locs&&locs.length>0? locs[0] : loc;
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
				 reject(err)
                 
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
     console.log(me.mPredicate);
    return new Promise((resolve, reject) => {
		console.log('start stream parser');
        let parser = new Parser();
        let flagOuter =false, flag = false, result = [];
        parser.on('opentag', (name, attrs) => {
            if(me.OUTER && name === me.OUTER){
                flagOuter = true;
            }
            if((!me.OUTER || flagOuter) && me.mPredicate.includes(name)){
                console.log(name)
                flag = true;
				if(name.includes('hasSubsystem') ||name.includes('owl:imports')|| name.includes('hasIRI')){//get rdf:resource//todo:hard code for now
					let text = attrs['rdf:resource'];
					if(text){
                        console.log(text);
					result.push(text);
					if(me.buffer &&!me.buffer.isPaused()) {me.buffer.push(text+'@'+(level))};
					flag = false;
					}
				}
            }
        });
        parser.on('text', (text) => {
            if(flag){
				if(text){
                result.push(text);
                //console.log(text)
                if(me.buffer &&!me.buffer.isPaused()) {me.buffer.push(text+'@'+(level))};
				}
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
            //console.log(loc+' pass validation');
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
    
        let res = request.get(loc, {timeout: 30000, agent: false}).on('error', function (err) {
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
    //console.log('query lvel: '+level)
    const self = this;
    switch(type){
        case 'endpoint':
            //construct query parameter
            console.log('query for end point')
            return new Promise((resolve, reject)=>{
                //run query against endpoint
				console.log(loc);
                //toodo: this is blasphemy, fix the endpoibt then delete this
                if(typeof self.queryStrC === 'string'){
                 self.queryStrC = [self.queryStrC];
                }    
                console.log('lenght of query'+self.queryStrC.length);
                async.map(self.queryStrC, self.singleEpQ(loc), function(err, resultArr){

                   if(err){
                    reject(err);
                    return;
                   }
                                   console.log('lenght of result'+resultArr.length);
                                   console.log(resultArr);

                     for(let idx = 0; idx< resultArr.length; idx++){
                        if(!resultArr[idx]){
                            continue;
                        }
                        for(let item of resultArr[idx]){
                            if(!'level'  in item){
                            item['level'] = idx+1;
                            console.log(item['level']);
                        }
                        }
                     }  

                    console.log('packed resulut:');
                    console.log([].concat.apply([], resultArr));
                  resolve([].concat.apply([], resultArr));
                } )
               


            });
            
            break;
            
        case 'local':
            //construct local graph then parse
            
            break;
            
        default:
            return new Error('Undefined query type')
    }
    

}

owlProcessor.singleEpQ = function(loc){

    let q = function(qStr, callback){
         
                    console.log('query')
                    console.log(qStr);
                   request.get(loc, {qs:{'query':qStr,'output':'json'},timeout: 150000, agent: false}, function (err, res, body) {
                    console.log('endpoint resquest result');
                                      //  console.log("body:"+body);

                    if (err||!body||body===undefined||body.toLowerCase().includes('doctype')||body.includes('<?xml version="1.0" encoding="utf-8"?>')) {
                        console.log('no result from endpoint, reject')
                        callback(err);
                        return;
                        };//don't throw
                    //unwrap query
                   
                    
                   //body = self.parsePseudoJson(body);
                   body = JSON.parse(body);
                   //todo: rewrite unwrap
                    let items = RdfParser.unwrapResult(body, 'item');
                     if(!items){
                        callback(new Error('empty query result'));
                        return;
                     }
                     
                   // let uri = items.map(item=>item.uri);
                    //console.log(uri)
                    //let tobuffer = uri.map((text)=> {return text+'@'+(level+1)}).join(';')
                    //self.buffer.push(tobuffer);
                         callback(null, items)
                })
               };

return q;
}

owlProcessor.checkJson = function(body){
    try{
    JSON.parse(body)
    return body
} catch(e){
    console.log(e)
    if(e){
    var re = /^(\{.*\})(.?)$/, re2=  /^(\[.*\])(.?)$/;
let formated = body.match(re)?body.match(re):body.match(re2);
return formated[1]
    }
}
}

owlProcessor.parsePseudoJson = function(body){
    let strs = body.split('\n');
    console.log(strs.length);
    strs =strs.filter((item)=>{return !(item.includes('[') || item.includes(']')||item.includes('{')||item.includes('}')) })
    .map(item=> '{'+item+'}');
    return JSON.parse('['+strs.join(',')+']');



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
		this.unpack = options.unpack?options.unpack:false;
        //modify query
        let extraQ = options.extraQuery?options.extraQuery:'';
        let extraP = options.extraPredicate?options.extraPredicate:'';
        let showImport = options.showImport || false; // if showServiceOnly is chosen, will not show Import
        this.mPredicate =  owlProcessor.PREDICATE.slice();
        if(showImport){
            console.log('show import')
            this.mPredicate.push("owl:imports");
            console.log(this.mPredicate);
            extraQ = extraQ + this.UnionImport;
        }
        console.log(extraQ)

        this.queryStrC = this.queryStr.replace('@placeholder@', extraQ);
                this.queryStr2C = this.queryStr2.replace('@placeholder@', extraQ);

        if(extraP){this.mPredicate.push(extraP);}

        if(options.supQuery){
            this.queryStrC = options.supQuery;
        }

        if(options.supPredicate){
            this.mPredicate = options.supPredicate;
        }

        this.OUTER = options.outer?options.outer:null;
        console.log(this.queryStrC);
    
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
	console.log(self.parentMap);
                let orphan = [];
                let foster= null;

    rawconn.forEach((link)=>{
		console.log(link)
        if(link.level === 1){
            firstl.push(link)
        } else{
            if(!(link.target in self.parentMap) ){
                    orphan.push(link)
            }
            else {
                            let parent = self.parentMap[link.target];

                if (parent in subconnections){
                subconnections[parent].connections.push(link)
            } else {
                if(!foster){foster = parent;}
                subconnections[parent] = {connections:[]}
			subconnections[parent].connections.push(link)

            }
        }
        }
    })
    console.log('orphans:')
     console.log(orphan);
    //subconnections[foster].connections = subconnections[foster].connections.concat(orphan);
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
	
		self.processed.add(self.diskLoc2Uri(this.loc));

      return new Promise((resolve, reject)=>{
          /*buffer logic**/
          this.buffer.on('data', (chunk) => {//receive new data:string of uris
              //process to get links
              //console.log(chunk.toString())
              let uris = chunk.toString().split(';');
              for (let item of uris){
                  if(item){
                      let uri = item.split('@');
                      let level = uri[1];
                      uri = uri[0];
					  console.log('after split:'+uri);
					  let auri = self.normalAddr(uri);
					  console.log('normailized: '+auri);
                      if(auri&&!self.processed.has(auri) ){
                          self.processed.add(auri);
                          self.doConnect(auri, parseInt(level)+1);//if find new uri, do connect
                      }
                  }
              }
        
          });
    
          this.buffer.on('end', () => {
              console.log('end of search');
              /*END OF ALL RETRIEVING************/
              //console.log(this.parentMap)
			  //self.destroy();
			  
              self.unpack?resolve(self.result):resolve(self.packIntoClusterData(self.result));
			              //  self.buffer = new Readable({read() {}});

          });
          this.buffer.on('close', () => {
              console.log('end of search. should return');
          });
          
      }).catch(err=>{
		  console.log(err)
	  })
    

        
      
    };

owlProcessor.processSingle = function (options) {
    this.init(options);
    return this.doConnect(this.loc, 1);
}

owlProcessor.diskLoc2Uri = function(disk){
	return disk?disk.replace('C:\\TOMCAT\\webapps\\ROOT\\', 'http://www.theworldavatar.com/').replace(new RegExp( '\\\\','g'), '/'):null;
}
owlProcessor.equalHost = function(url){
	return url?url.replace('http://www.jparksimulator.com/', 'http://www.theworldavatar.com/'):null;
}

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

var a = Object.create(owlProcessor)
    a.processSingle( {topnode:"http://localhost/damecoolquestion/ontochem/query", level: 1}).then((res)=>{
        console.log('print results')
        console.log((res))});

//b.process( {topnode:"http://www.theworldavatar.com/SemakauIsland.owl"}).then((res)=>{
  //  console.log('print')
    //console.log((res))});

module.exports = owlProcessor;
