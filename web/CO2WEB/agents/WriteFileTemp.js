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
                    let loc = config.root+"/"+path

         console.log(`temp: ${temp}`)
                  console.log(`mypath: ${loc}`)
                           console.log(`attr: ${attrs}`)



        copyTemp(temp, loc, ()=>{
             //TODO, delete old one
            // todo:SPARQLStr.constructOwlDef()
            let url = config.baseUri+"/"+path
            
            let QList = SPARQLStr.constructOwlDef({url, imports:attrs.imports||[]})

            //TODO:url most be modified in a file kind of way, because new uri did not exist yet


            
            if(attrs.x &&  attrs.y) {
               QList= QList.concat(SPARQLStr.constructCoordinate({name: path, url, x: attrs.x, y: attrs.y}))
            }
            
            if(attrs.capacity) {
                QList = QList.concat(SPARQLStr.constructCapacity({name: path, url, value: attrs.capacity}))
            }
            //todo, rest of the attrs
            
            let uriList = Array(QList.length).fill(url)

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