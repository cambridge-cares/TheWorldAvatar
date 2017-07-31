/***
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
//import
var path = require('path');
var libxmljs = require("libxmljs");
var proj4 = require('proj4');


var async = require('async');
var readdirp = require('readdirp');
var fs = require('fs');
var util = require('util');
var config = require('../config.js');
//var folderLocation = config.root;
let request = require('request');
/**out a asyn function, provide data :
 [
 {source: , target: , level: }
 ]


 ****/

//var connections = [];

/**
 *
 * @param options  topnode(topnode address on disk), [showServiceOnly(bool) } showImport(bool)]
 * @param callback  fn(err, results)
 */
function readConnections(options, callback) {

    let showServiceOnly = options.showServiceOnly || false;
    let showImport = options.showImport&&!showServiceOnly || false; // if showServiceOnly is chosen, will not show Import
    let fileLocation = options.topnode;
    if(!fileLocation){
        callback(new Error("top node not specified"));
        return;
    }


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

        //read root file on disk
        fs.readFile(fileLocation, function (err, file) {
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

         //Get every tab defined in ontology tab
              let imports =  root.find('//owl:Ontology//owl:imports', {owl:'http://www.w3.org/2002/07/owl#', rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#resource"});
              console.log(imports);


                let results = [];
              if(imports ) {
                  results = imports.map(function (item) {

                      console.log(item.toString())

                      console.log("resource:!!!!!!" + item.attr("resource").value());

                      return item.attr("resource").value();
                  });

              }
            return results;
        }
        catch (err) {
            throw  err;
        }
    }
	
	function getUrl(root){
		            for (var ns of root.namespaces()) {
                //console.log(ns.prefix()+ ":"+ns.href());
                //get base from xml
                if (ns.prefix() === null) {//if prefix not null [ self namespace is defined with null prefix]
                    console.log("prefix:" + ns.prefix());

                  return ns.href();
                }
            }
			
			return null;
	}


    function getChildren(root) {
		if(!root){
			
			return [];
		}
        var children = [];
        var namespaceOb = {};//construct namespaceOb for find in root with nested namespace

        namespaceOb['owl'] = "http://www.w3.org/2002/07/owl#";
        namespaceOb['Eco-industrialPark'] = "http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl#";
        namespaceOb['system'] = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#";

        console.log(util.inspect(namespaceOb));

        //find all node with hasIRI property
        var uris = root.find("//owl:NamedIndividual/Eco-industrialPark:hasIRI", namespaceOb);
        console.log("found node Eco-industrialPark:hasIRI:" + uris.length);
        for (let curi of uris) {
           // console.log(curi.name());
            children.push(curi.text().trim());//push to targets list
        }
        //find all node with SYSTEM:hasIRI property
        let urisS = root.find("//system:hasIRI", namespaceOb);
        console.log("found node system:hasIRI:"+urisS.length);
        for(let curi of urisS){
            //    console.log(curi.name());
            children.push(curi.text().trim());//push to targets list
        }
        return children;
    }

    function getGeoCoord(root) {
        if(!root){
            return null;
        }
        let x =  root.find("//owl:NamedIndividual[contains(@rdf:about, 'ValueOf_x_')]", {owl:'http://www.w3.org/2002/07/owl#', rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#"});

        let y =  root.find("//owl:NamedIndividual[contains(@rdf:about, 'ValueOf_y_')]", {owl:'http://www.w3.org/2002/07/owl#', rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#"});

        if(x.length > 0 && y.length > 0) {
            console.log("#########################findcoordis:" + x[0].text().trim());
            console.log("converted coordi: " +  util.inspect(convertCoordinate(x[0].text().trim(), y[0].text().trim(), false)));
            return convertCoordinate(x[0].text().trim(), y[0].text().trim(), false);
        } else {
            return null;
        }
        }

    //convert google GPS coordi to 1984w coordi(the one used in our own)
    var convertCoordinate = function (GPSLong, GPSLat, google2Owl) {
//https://github.com/proj4js/proj4js
        var googleProjection = 'EPSG:4326'; //google
        var ourProjection = 'EPSG:3857';//our
//console.log("convert coordis: ["+parseInt(GPSLong)+", "+parseInt(GPSLat)+"] to "+proj4(fromProjection, toProjection, [parseInt(GPSLong),parseInt(GPSLat)]));

        return google2Owl?converted(googleProjection, ourProjection) : converted(ourProjection, googleProjection);
        function converted(fromProjection, toProjection){

            var result =  proj4(fromProjection, toProjection, [parseFloat(GPSLong),parseFloat(GPSLat)]);

            return {x: result[0], y:result[1]};
        }

    };

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
        console.log("found Service node :"+uris.length);

        for(let curi of uris){
            //    console.log(curi.name());
            services.push(curi.text().trim());//push to targets list
        }
        return services;

    }



       function loopChildrenRecur(file, level, callback) {

	   
		       console.log("------------loopChildRecur--------------------");
			   var root;
			      try{
               let xmlDoc = libxmljs.parseXml(file);
                root = xmlDoc.root();
               var myUri = (root.attrs() && root.attrs().length > 0) ? root.attrs()[0].value() : null;
               
			   if(myUri === null){
			    myUri = getUrl(root);
			   }
               console.log("myURI" + myUri);
               //TODO: request for remote  VS search with name on current?
               //TODO: xml parse the file => get targets list == childList => request on each child file

				  } catch(err){
					  console.log(err);
				  }
               let children = getChildren(root);
           //get my links
           let myLinks = [];
           let geoCoords = [];
           var imports;

           //get this geoCoord, push it on result links
           var coord = getGeoCoord(root);
           if(coord) {
               geoCoords.push({url : myUri, coord: coord});
           }

           console.log(util.inspect(geoCoords));
           if (showImport) {
               try {
                    imports = getXMLImports(root);
                   for (let imported of imports) {
                       console.log("import iri:" + imported);
                       myLinks.push({source: myUri, target: imported,level:null})
                      // children.push(imported);
                   }
                   ;

               } catch (err) {
                   callback(err);
                   return;
               }
           }

           if (children.length < 1) { // no children is found, including devices and services and call callback
               console.log(myUri + " is a leaf node return");

               callback(null, {connections: myLinks, geoCoords: geoCoords});
               return;
           }//else




               //push all children into link array

               if(showServiceOnly) { //pack only services into results
                   var services = getServices(root);
                   for (let service of services) {
                       console.log("service iri: " + service);
                       myLinks.push({source: myUri, target: service});
                   }

               }  else {
                   for (let target of children) {
                       console.log("child iri: " + target);
                       myLinks.push({source: myUri, target: target, level: level});
                   }
               }
           if (showImport) {
               children = children.concat(imports);
           }
               //request on each child, execute loopChild on it, then concate the result
               async.concat(children, requestChild, function (err, results) {
                   if (err) {
                       console.log("concat err");
                       console.log(err.message);
                       callback(err);
                       return;
                   }
                   console.log("concat results");


                   let connectionsAll = myLinks;
                   let geoCoordsAll = geoCoords;

                   console.log("!!!!!!!!!!!!!!!!!!!!!" + results.length);
                   //console.log(geoCoords)
                   for (let result of results){
                       console.log(util.inspect(result));

                       let connectionThis = result.connections;
                       //console.log(connectionThis);
                     //  connectionThis = connectionThis.clean(null);
                       let geoCoordsThis = result.geoCoords;
                       console.log(util.inspect(geoCoordsThis));

                       //  geoCoordsThis = geoCoordsThis.clean(null);
                       connectionsAll = connectionsAll.concat(connectionThis);
                       geoCoordsAll=   geoCoordsAll.concat(geoCoordsThis);
                   }


                   console.log("********************concated");
                   console.log(util.inspect(connectionsAll));
                   console.log(util.inspect(geoCoordsAll));

                   callback(null, {connections:connectionsAll, geoCoords:geoCoordsAll});

               });



           function requestChild(iri, callback) {//http request  to get child file
               request.get(iri, {timeout: 2000, agent:false},function (err, response, body) {

                   if (err || response.statusCode !== 200) { //request failed
                       console.log(err);
                       callback(null, null); // return null
                       return;

                   }

                   if (response.statusCode === 200) {//request success
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
            if (this[i] === deleteValue) {
                this.splice(i, 1);
                i--;
            }
        }
        return this;
    };


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
