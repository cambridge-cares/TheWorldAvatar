/***
This module is used for GAMS showcase. Generate plant owl file according to premade csv result files.
***/
const fs = require('fs'),
    csv = require('csv'),
    writefromTemp = require('./WriteFileTemp'),
    SPARQLStr = require('./SPARQLStr'),
    SPARQLEpReq = require('./SPARQLEpReq'),
    config = require("../config"),
path =  require('path'),

async = require('async')


function runGAMSPredefined(id, cb){
    console.log("id: "+id)
    console.log(typeof  id)
    //params is predefined id
    let filename;
    switch(id){
       case 0:
           filename = "./agents/results.csv"
           break;
           
       case 1:
           filename = "./agents/results2.csv"
           break;   

     case 2:
          filename =   "./agents/results3.csv" 
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
                    name:'nuc'+item[0],
					uri:'http://www.jparksimulator.com/kb/sgp/jurongisland/nuclearpowerplants/nuc'+item[0]+'.owl',

                    capacity:item[1],
                    location:{lat:parseFloat(item[2]), lng:parseFloat(item[3])},
                    type: "nuclear"
                }
            })

            console.log(csvresult)
             //return result before wrting back to files
            cb(null, csvresult)


            //[{tempLoc, attrList, newFileName}]
            
            /*call API to delete old files, since API not completed, do it natively for now**/
            //fs.readdir(config.root, (err, files)=>{
			fs.readdir("C:/TOMCAT/webapps/ROOT/kb/sgp/jurongisland/nuclearpowerplants", (err, files)=>{	
                if(err)
                    throw err
                
                let oldPPfiles = files.filter((file)=>{
                    return file.match(/^nucj\d+\.owl$/g)
                })
                
                console.log(oldPPfiles)
                //oldPPfiles = oldPPfiles.map( name=> path.join(config.root, name))
				oldPPfiles = oldPPfiles.map( name=> path.join("C:/TOMCAT/webapps/ROOT/kb/sgp/jurongisland/nuclearpowerplants", name))
                
                //delete them all natively
                async.each(oldPPfiles, fs.unlink, (err)=>{
                        
                    //SPARQLStr.constructNDeletes()
    
                    let uriList = [], QList = []
                    let parenturi = "http://www.jparksimulator.com/kb/sgp/jurongisland/nuclearpowerplants/NuclearPowerPlants.owl";
                    let baseUri = config.baseUri;
                    /*call API to update the top node****/
                    let deleteStr = SPARQLStr.construct('delete', '<http://www.jparksimulator.com/kb/sgp/jurongisland/nuclearpowerplants/NuclearPowerPlants.owl#JurongIslandNuclearPlants>','system:hasSubsystem','?o', {'system':'http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#'});
                    console.log(deleteStr)
                    QList.push(deleteStr)
					let deleteStr2 = SPARQLStr.construct('delete', '<http://www.jparksimulator.com/kb/sgp/jurongisland/nuclearpowerplants/NuclearPowerPlants.owl#JurongIslandNuclearPlants>','j.0:hasSubsystem');
                    console.log(deleteStr2)
                    QList.push(deleteStr2)
                    csvresult.forEach((item) =>{
                        let childuri = baseUri+"/kb/sgp/jurongisland/nuclearpowerplants/"+item.name+'.owl';
                        let updateStr=  SPARQLStr.construct('insertdata', '<http://www.jparksimulator.com/kb/sgp/jurongisland/nuclearpowerplants/NuclearPowerPlants.owl#JurongIslandNuclearPlants>','<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem>', '<'+childuri+'>');
                        //let updateStr=  SPARQLStr.construct('insertdata', '<http://www.jparksimulator.com/kb/sgp/jurongisland/nuclearpowerplants/NuclearPowerPlants.owl#JurongIslandNuclearPlants>','<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl##hasSubsystem>','<${childuri}#${item.name}>' );
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
                        
                    })
    
    
    
					//let tempLoc = path.join(config.root, 'powerplantTemp.owl');
                    let tempLoc = path.join("C:/TOMCAT/webapps/ROOT/kb/sgp/jurongisland/nuclearpowerplants", 'powerplantTemp.owl');
                    //{temp, name, attrs[{s,p,o}]}
    
                    let packed = csvresult.map((item)=>{
                        let childuri = baseUri+"/kb/sgp/jurongisland/nuclearpowerplants/"+item.name + '.owl';
                        let path = item.name+'.owl'
						let attrs = []
                        //capacity, xcoord, ycoord
                        attrs['x'] = item.location.lng
                        attrs['y'] = item.location.lat
                        attrs['capacity'] = item.capacity
                        attrs['imports'] = ["http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl","http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl",
						"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl",
						"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl"]
                        console.log(attrs)

                        return {temp: tempLoc, path:path, attrs:attrs }
                        
                    })
    
    
                    
    
                    //werite new files
                    async.each(packed, writefromTemp.createFile,(err)=>{
                        //console.log("error in gamsowl="+err)
                       // cb(err)
                    } )
                
           
    
                })
                
                
            })
            

        })

    })
}


module.exports = runGAMSPredefined