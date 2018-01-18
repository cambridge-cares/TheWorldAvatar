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
        copyTemp(temp, path, ()=>{
             //TODO, delete old one
            // todo:SPARQLStr.constructOwlDef()
            let url = config.baseUri+"/"+path
            
            let QList = SPARQLStr.constructOwlDef({url, imports:attrs.imports||[]})
            if(attrs.x &&  attrs.y) {
                QList.concat(SPARQLStr.constructCoordinate({name: path, url, x: attrs.x, y: attrs.y}))
            }
            
            if(attrs.capacity) {
                QList.concat(SPARQLStr.constructCapacity({name: path, url, value: attrs.capacity}))
            }
            //todo, rest of the attrs
            
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