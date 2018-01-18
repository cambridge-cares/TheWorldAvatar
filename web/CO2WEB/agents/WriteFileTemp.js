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
         console.log(`temp: ${temp}`)
                  console.log(`mypath: ${path}`)
                           console.log(`attr length: ${attrs.length}`)


        copyTemp(temp, path, ()=>{
             //TODO, delete old one
             let QList = attrs.map(({s,p,o})=>{
                 return   SPARQLStr.construct('insertdata', s, p,o);
             })
             let baseUri   = config.baseUri+path
             let uriList = Array(QList.length).fill(baseUri)
    
    
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