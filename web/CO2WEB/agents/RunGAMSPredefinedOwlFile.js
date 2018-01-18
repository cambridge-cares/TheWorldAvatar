
const fs = require('fs'),
    csv = require('csv'),
    writefromTemp = require('./WriteFileTemp'),
    SPARQLStr = require('./SPARQLStr'),
    SPARQLEpReq = require('./SPARQLEpReq'),
    config = require("../config"),
path =  require('path'),

async = require('async')


function runGAMSPredefined(id, cb){
    //TODO: read csv according to given params
    console.log("id: "+id)
    console.log(typeof  id)
    //params is predefined id
    let filename;
    switch(id){
        case 0:
            filename = "./agents/results.csv"
            break;
        default:
            throw new Error("non-existing predefined resultset id");
    }
    console.log(filename)

    if(!filename){
        throw new Error("non-existing predefined resultset id")
    }

    fs.readFile(filename, function (err ,rdata) {
        if(err){
            console.log(err)
            cb(err)
            return
        }
        csv.parse(rdata, function (err, data) {

            if(err){
                cb(err)
                console.log(err)
                return
            }
            data.splice(0,1)
            let csvresult = data.map((item) =>{
                return {
                    uri:'nuc'+item[0],
                    capacity:item[1],
                    location:{lat:parseFloat(item[2]), lng:parseFloat(item[3])},
                    type: "nuclear"
                }
            })

            console.log(csvresult)

            //[{tempLoc, attrList, newFileName}]
            
            /*call API to delete old files, since API not completed, do it natively for now**/
            fs.readdir(config.root, (err, files)=>{
                if(err)
                    throw err
                
                let oldPPfiles = files.filter((file)=>{
                    return file.match(/^nucj\d+\.owl$/g)
                })
                
                console.log(oldPPfiles)
                oldPPfiles = oldPPfiles.map( name=> path.join(config.root, name))
                //delete them all natively
                async.each(oldPPfiles, fs.unlink, (err)=>{
                    
                    //todo: err handling
    
                    //SPARQLStr.constructNDeletes()
    
                    let uriList = [], QList = []
                    let parenturi = "http://www.theworldavatar.com/NuclearPlants.owl";
                    let baseUri = config.baseUri;
                    /*call API to update the top node****/
                    let deleteStr = SPARQLStr.construct('delete', '<http://www.w3.org/2002/07/owl#NamedIndividual>','Eco-industrialPark:hasIRI','?o', {'Eco-industrialPark':'http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl#'});
                    console.log(deleteStr)
                    QList.push(deleteStr)
                    csvresult.forEach((item) =>{
                        let childuri = baseUri+"/"+item.uri+'.owl';
                        let updateStr=  SPARQLStr.construct('insertdata', '<http://www.w3.org/2002/07/owl#NamedIndividual>','<http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl#hasIRI>',{value:childuri, type:"string"});
                        QList.push(updateStr)
                        console.log(updateStr)

                    })
    
                    for(let i = 0; i < QList.length;i++){
                        uriList.push(parenturi)
                    }
                    //construct urilist
                    //construct updateQs
                    SPARQLEpReq.call(uriList, QList, (err)=>{
                        console.log("")
                        if (err)
                        throw err;
                        
                        cb(null, csvresult)
                    })
    
    
    
    
                    let tempLoc = path.join(config.root, 'powerplantTemp.owl');
                    //{temp, name, attrs[{s,p,o}]}
                    //TODO:　pack into above format
    
                    let packed = csvresult.map((item)=>{
                        let childuri = baseUri+"/"+item.uri + '.owl';
                        let path = item.uri+'.owl'
                        let attrs = []
                        //capacity, xcoord, ycoord
                        attrs['x'] = item.location.lng
                        attrs['y'] = item.location.lat
                        attrs['capacity'] = item.capacity
                        attrs['imports'] = ["http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl","http://www.theworldavatar.com/OntoEIP/Eco-industrialPark.owl"]
                        //TODO: here
                        console.log(attrs)

                        return {temp: tempLoc, path:path, attrs:attrs }
                        
                    })
    
    
                    
    
                    async.each(packed, writefromTemp.createFile,(err)=>{
                        console.log(err)
                        cb(err)
                    } )
                
           
    
                })
                
                
            })
            

        })

    })
}


module.exports = runGAMSPredefined

