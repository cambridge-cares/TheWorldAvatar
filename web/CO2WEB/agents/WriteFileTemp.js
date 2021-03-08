/**
 * Create a new file by tempalte
 */

const util = require('util')
const fs = require('graceful-fs'),
    path = require('path'),
    config = require('../config'),
    SPARQLStr = require('./SPARQLStr'),
    SPARQLEpReq = require('./SPARQLEpReq'),
    request = require('request'),
    async = require('async')

//rdflib does not have support for writing, need to use endpoint API

//attr: name, type, value

var WriteFileTemp = {



    //attrs[{s, p, o}]
     createFile:function({temp, path, attrs}, cb){
//construct update string according to attrs
         //pack attr list
                    let loc = config.root+"/kb/sgp/jurongisland/nuclearpowerplants/"+path

         console.log(`temp: ${temp}`)
                  console.log(`mypath: ${loc}`)
                           console.log(`attr: ${attrs}`)



        copyTemp(temp, loc, ()=>{
             //TODO, delete old one
            // todo:SPARQLStr.constructOwlDef()
            console.log("now modify it")

            //let url = config.baseUri+"/kb/sgp/jurongisland/nuclearpowerplants/"+path
			let url = "http://www.jparksimulator.com"+"/kb/sgp/jurongisland/nuclearpowerplants/"+path
            console.log(url)

            let QList = SPARQLStr.constructOwlDef({uri:url, imports:attrs.imports||[]})


                  console.log(attrs)

            if(attrs.x &&  attrs.y) {
               QList= QList.concat(SPARQLStr.constructCoordinate({name: path, url:url, x: attrs.x, y: attrs.y}))
			}
            
            if(attrs.capacity) {
                //TODO: a bug here , probably too many requests at one time, probably this is wrong
             //   QList = QList.concat(SPARQLStr.constructCapacity({name: path, url:url, value: attrs.capacity}))
            }
            //todo, rest of the attrs
            
            let uriList = Array(QList.length).fill(url)

            //console.log(QList)
			console.log("this is QLIST= "+QList)


             SPARQLEpReq.call(uriList, QList, (err)=>{
                 if (err)
                 cb(err)
                 
                 cb(null, 'success');
             })
         });
    
    
          function copyTemp(temp, path, cb) {
             //stream tempfile to a copy with designated new name
             let s = fs.createReadStream(temp).pipe(fs.createWriteStream(path));
             s.on('finish', cb)
         }

//callback with success/error


},

    



}



module.exports = Object.create(WriteFileTemp)