/***
 Dated version of owl parser. xml processing style. 
 * Read all connecitons defined in current grOWl system, starting from a top node file on disk
 * All following children will be retreive by url online
 * Following definition will be considered a connection:
 *  //owl:NamedIndividual/Eco-industrialPark:hasIRI
 *  //owl:NamedIndividual/system:hasIRI
 *
 *   option: showImport
 *   Following is also considered connection if showImport ticked
 *   owl:Ontology//owl:imports
 *
 *   option: showServiceOnly
 *   Connection is logged only when
 *   //owl:NamedIndividual[rdf:type[contains(@rdf:resource,'http://www.theworldavatar.com/Service.owl')]]
 *
 *   Connections are saved as:
 *   [{source, target: , level: },..]
 *   Import connections only have level: null
 
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
/**out a asyn function, provide data :
 [
 {source: , target: , level: }
 ]
 ****/
//commented out lazy innitiation
//var connections = [];

var owlProcessor = {};

/**
 * main function, async
 * @param options  topnode(topnode address on disk), [showServiceOnly(bool) } showImport(bool)]
 * @param callback  fn(err, results)
 */
owlProcessor.getChildrenRecur = function(options, callback) {
    
    /*settings***********************/
    let showServiceOnly = options.showServiceOnly || false;
    let showImport = options.showImport&&!showServiceOnly || false; // if showServiceOnly is chosen, will not show Import
    let showServiceUrl = options.showServiceUrl  || false;
    let topnodeLocation = options.topnode;
    if(!topnodeLocation){
        callback(new Error("top node not specified"));
        return;
    }
    
    
    let nodeMap = new Set();
    //commented out lazy initiation
    // if (connections && connections.length > 0) {
    //      callback(null, connections);
    // } else {
    
    /*start from a root node recursively read connections from child files*******/
    startFromRoot2GetConns(function (err, connections) {
        if (err) {
            callback(err);
            return;
        }
        callback(null, connections);
    });
    
    //  }
    
    
    /***
     *start from a root node recursively read connections from child files
     * @param callback(err result)
     */
    function startFromRoot2GetConns(callback) {
        
        logger.debug("fileConnection: !!!! start reading file:" + topnodeLocation);
        /*read top node**/
        fs.readFile(topnodeLocation, function (err, file) {
            
            
            if (err) {
                logger.debug("fileConnection: errReadingFile");
                callback(err);
                throw err;
                return;
            }
            
            
            /*retreive child info from file and recursively read through children**/
            loopChildrenRecur(file, 0, callback);
            
        })
        
    }
    
    
    
    
    /**
     * return all services defined in the owl, service definition : contains "http://www.theworldavatar.com/Service.owl" in rdf:type
     * @param root   root node of parsed xml
     * @returns {*}  an array of services urls
     */
    function getServices(root){
        
        if(!root){
            
            return [];
        }
        var services = [];
        var namespaceOb = {};//construct namespaceOb for find in root with nested namespace
        namespaceOb['owl'] = "http://www.w3.org/2002/07/owl#";
        
        namespaceOb["rdf"] = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
        namespaceOb['Eco-industrialPark'] = "http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl#";
        
        var uris = root.find("//owl:NamedIndividual[rdf:type[contains(@rdf:resource,'http://www.theworldavatar.com/Service.owl')]]", namespaceOb);
        //  logger.debug("found Service node :"+uris.length);
        
        for(let curi of uris){
            //    logger.debug(curi.name());
            services.push(curi.text().trim());//push to targets list
        }
        return services;
        
    }
    
    
    
    function loopChildrenRecur(file, level, callback) {
        
        
        
        logger.debug("------------loopChildRecur--------------------");
        // console.log(file);
        var root = owlProcessor.parseXMLFile(file);
        
        if(!root){
            callback(null,null);
            return;
        }
        var myUri = owlProcessor.getUri(root);
        logger.debug(myUri)
        nodeMap.add(myUri);
        let children = owlProcessor.getChildren(root);
        //get my links
        let connectionParent = [],geoCoordsParent = [],serviceUrlsParent = [];
        var imports;
        
        var coord = owlProcessor.getGeoCoord(root);
        //get this geoCoord, push it on result links
        if(coord) {
            let type = owlProcessor.getType(root);
            console.log("type: "+ type)
            geoCoordsParent.push({url : myUri, coord: coord, type:type});
        }
        
        if(showServiceUrl){
            let serviceUrltmp = owlProcessor.getServiceUrl(root);
            if(serviceUrltmp){
                logger.debug("found service url: " +serviceUrltmp)
                serviceUrlsParent.push({url : myUri, serviceUrl: serviceUrltmp})
            }
            
        }
        if (showImport) {
            try {
                imports = owlProcessor.getXMLImports(root);
                for (let imported of imports) {
                    //   logger.debug("import iri:" + imported);
                    connectionParent.push({source: myUri, target: imported,level:null})
                    // children.push(imported);
                }
                children.concat(imports)
                ;
                
            } catch (err) {
                callback(err);
                return;
            }
        }
        
        // logger.debug("current node map: ")
        // logger.debug(nodeMap)
        children = children.filter(function (childUri) {
            return !(nodeMap.has(childUri));
        });
        if (children.length < 1) { // no children is found, including devices and services and call callback
            logger.debug(myUri + " is a leaf node return");
            callback(null, {connections: connectionParent, geoCoords: geoCoordsParent, serviceUrls: serviceUrlsParent});
            return;
        }//else
        
        //push all children into link array
        
        if(showServiceOnly) { //pack only services into results
            var services = getServices(root);
            for (let service of services) {
                //  logger.debug("service iri: " + service);
                connectionParent.push({source: myUri, target: service});
            }
            
        }  else {
            for (let target of children) {
                //  logger.debug("child iri: " + target);
                connectionParent.push({source: myUri, target: target, level: level});
            }
        }
        
        
        //request on each child, execute loopChild on it, then concate the result
        async.map(children, requestChild, function (err, results) {
            if (err) {
                // logger.debug("concat err");
                logger.debug(err.message);
                callback(err);
                return;
            }
            
            if (level === 0){
                //pack results
                var subconnections = {}, idx = 0;
                for (let idx = 0; idx< children.length; idx++){
                    let iri = children[idx]
                    subconnections[iri] = results[idx];
                }
                callback(null, {connections: connectionParent, subconnections: subconnections})
                return;
            }
            logger.debug("concat results");
            
            let connectionsAll = connectionParent;
            let geoCoordsAll = geoCoordsParent;
            let serviceUrlAll = serviceUrlsParent;
            
            
            //logger.debug(geoCoordsParent)
            console.log(results)
            for (let result of results){
                if(result) {
                    let connectionChild = result.connections;
                    let geoCoordsChild = result.geoCoords;
                    let serviceUrlChild = result.serviceUrls;
                    //  geoCoordsThis = geoCoordsThis.clean(null);
                    connectionsAll = connectionsAll.concat(connectionChild);
                    geoCoordsAll = geoCoordsAll.concat(geoCoordsChild);
                    serviceUrlAll = serviceUrlAll.concat(serviceUrlChild);
                }
           
                }
            //logger.debug("********************concated");
            // logger.debug(util.inspect(connectionsAll));
            // logger.debug(util.inspect(geoCoordsAll));
            //  logger.debug("All service urls data:")
            //  logger.debug(JSON.stringify(serviceUrlAll))
            
            callback(null, {connections:connectionsAll, geoCoords:geoCoordsAll, serviceUrls:serviceUrlAll});
            
        });
        
        
        
        function requestChild(iri, callback) {//http request  to get child file
            request.get(iri, {timeout: 100000, agent:false},function (err, response, body) {
                
                if (err || response.statusCode !== 200) { //request failed
                    logger.debug(err);
                    callback(null, null); // return null
                    return;
                    
                }
                
                if (response.statusCode === 200) {//request success
                    //logger.debug("req: " + iri);
                    logger.debug("!!!!!request child:"+iri);
                    loopChildrenRecur(body, level+1, callback);
                    return;
                }
                
                logger.debug("why the fuck did anyone get to this point");
                callback(null, null);
            });
            
        }
        
        
    }
    

}


owlProcessor.parseXMLFile = function(file) {
    try{
        let xmlDoc = libxmljs.parseXml(file);
        let root = xmlDoc.root();
        // var myUri = (root.attrs() && root.attrs().length > 0) ? root.attrs()[0].value() : null;
        
        return root;
        
        //     logger.debug("myURI" + myUri);
        
    } catch(err){
        logger.debug(err);
    }
}

owlProcessor.getXMLImports = function(root) {
    try {
        
        //Get every tab defined in ontology tab
        let imports =  root.find('//owl:Ontology//owl:imports', {owl:'http://www.w3.org/2002/07/owl#', rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#resource"});
        //   logger.debug(imports);
        let results = [];
        if(imports ) {
            results = imports.map(function (item) {
                return item.attr("resource").value();
            });
            
        }
        return results;
    }
    catch (err) {
        throw  err;
    }
}

owlProcessor.getUri = function(root){
    
    let uri = root.find('//owl:Ontology', {owl:'http://www.w3.org/2002/07/owl#'});
    if(!uri || uri.length < 1){
        return null;
    }
    
    return uri[0].attr("about").value();
    
}



/**
 * A link to the service page is defined in the service owl. this function retrieves it
 * @root  xml root of the file
 */
owlProcessor.getServiceUrl = function(root) {
    
    let urls  = root.find("//Service:hasUrl", {"Service" :"http://www.theworldavatar.com/Service.owl#"});
    
    if(!urls || urls.length < 1){
        
        return null;
    }
    // logger.debug("found service url in func: " +urls[0].text().trim())
    
    return  urls[0].text().trim();
}


/**
 * Find all children defined in an owl file
 * @param root
 * @returns {Array}
 */
owlProcessor.getChildren = function(root) {
    if(!root){
        return [];
    }
    var children = [];
    var namespaceOb = {};//construct namespaceOb for find in root with nested namespace
    
    namespaceOb['owl'] = "http://www.w3.org/2002/07/owl#";
    namespaceOb['Eco-industrialPark'] = "http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl#";
    namespaceOb['system'] = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#";
    
    //find all node with hasIRI property
    var uris = root.find("//Eco-industrialPark:hasIRI", namespaceOb);
    //  logger.debug("found node Eco-industrialPark:hasIRI:" + uris.length);
    for (let curi of uris) {
        // logger.debug(curi.name());
        if(curi.text().trim()!==""){
            children.push(curi.text().trim());//push to targets list
        } else if(curi.attr("resource").value()){
            children.push(curi.attr("resource").value());//push to targets list
        }
        
        
    }
    //find all node with SYSTEM:hasIRI property
    let urisS = root.find("//system:hasIRI", namespaceOb);
    //logger.debug("found node system:hasIRI:"+urisS.length);
    for(let curi of urisS){
        //    logger.debug(curi.name());
        children.push(curi.text().trim());//push to targets list
    }
    
    
    //delete # (location part)
    children = children.map((uri)=>{
        return uri.split("#")[0]
    })
    return Array.from(new Set(children));
}

owlProcessor.getPPChildren = function (root) {
    if(!root){
        return [];
    }
    var children = [];
    var namespaceOb = {};//construct namespaceOb for find in root with nested namespace
    // namespaceOb['system'] = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#";
    // namespaceOb['f'] ="http://www.theworldavatar.com/TheWorld.owl#";
    //find all node with SYSTEM:hasIRI property
    let uris = root.find("//self:PowerPlant//system:hasIRI", owlProcessor.getNSList(root));
    //logger.debug("found node system:hasIRI:"+urisS.length);
    
    // logger.debug("find pp:"+uris.length)
    
    for(let curi of uris){
        //    logger.debug(curi.name());
        //logger.debug(JSON.stringify(curi.path()))
        children.push(curi.text().trim());//push to targets list
    }
    
    return children;
}

/***
 * @param root
 * @returns {*}
 */
owlProcessor.getGeoCoord = function(root) {
    if(!root){
        return null;
    }
    let x =  root.find("//*[contains(@rdf:about, '_x_')]", {owl:'http://www.w3.org/2002/07/owl#', rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#"});
    let y =  root.find("//*[contains(@rdf:about, '_y_')]", {owl:'http://www.w3.org/2002/07/owl#', rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#"});
    
    if(x.length > 0 && y.length > 0) {
        // logger.debug("#########################findcoordis:" + x[0].text().trim());
        // logger.debug("converted coordi: " +  util.inspect(convertCoordinate(x[0].text().trim(), y[0].text().trim(), false)));
        return owlProcessor.convertCoordinate(x[0].text().trim(), y[0].text().trim(), false);
    } else {
        return null;
    }
};


owlProcessor.getType = function (root) {
    if(!root){
        return null;
    }
    let x =  root.find("//rdf:type[contains(@rdf:resource, 'EN_realization.owl')]", {owl:'http://www.w3.org/2002/07/owl#', rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#"});
    
    let type;
    if(x.length > 0){
        console.log("typeuri: "+ x[0].attr("resource").value())
        let uriArr = x[0].attr("resource").value().split('#');
        if(uriArr.length > 1){
            type = uriArr[uriArr.length-1];
        }
    }
    
    if(type){
        
        return type;
        
    } else {
        let uri = owlProcessor.getUri(root);
        let regex = /^.*\/(\w+)[-_]\d+\.owl$/;
        let match = regex.exec(uri);
        return match?match[1]:null;
        
    }
    
}
//convert google GPS coordi to 1984w coordi(the one used in our own)
owlProcessor.convertCoordinate = function (GPSLong, GPSLat, google2Owl) {
//https://github.com/proj4js/proj4js
    var googleProjection = 'EPSG:4326'; //google
    var ourProjection = 'EPSG:3857';//our
//logger.debug("convert coordis: ["+parseInt(GPSLong)+", "+parseInt(GPSLat)+"] to "+proj4(fromProjection, toProjection, [parseInt(GPSLong),parseInt(GPSLat)]));
    
    return google2Owl?converted(googleProjection, ourProjection) : converted(ourProjection, googleProjection);
    function converted(fromProjection, toProjection){
        
        GPSLong  = typeof GPSLong === "string"?parseFloat(GPSLong) : GPSLong
        GPSLat  = typeof GPSLat === "string"?parseFloat(GPSLat) : GPSLat
        
        var result =  proj4(fromProjection, toProjection, [GPSLong,GPSLat]);
        
        return {x: result[0], y:result[1]};
    }
    
};

owlProcessor.getNSList = function (root) {
    //each one in ns, put on nsList
    let ns = root.namespaces();
    let list = {}
    ns.forEach(function (item) {
        list[item.prefix()===null?"self":item.prefix()] = item.href();
    });
    // logger.debug(JSON.stringify(list))
    return list;
};

owlProcessor.uriList2DiskLoc = function (uriArr, diskroot) {
    diskroot = diskroot || config.root;
    return uriArr.map(function (item) {
        // logger.debug("map:"+item)
        let diskLoc = item.replace("http://www.theworldavatar.com",diskroot);
        diskLoc = diskLoc.replace("http://www.jparksimulator.com",diskroot);
        return {uri:item, diskLoc:diskLoc}
    });
}

module.exports = owlProcessor;
