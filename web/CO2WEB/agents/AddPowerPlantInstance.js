/**
 * Created by Shaocong on 9/4/2017.
 * A simple tool to add new Powerplant instance to theworld owlfile
 *
 */

var config = require('../config')
    ,path =require('path')
    ,fs = require('fs')
    ,worldNode =config.worldNode
    ,root = config.root
    ,xmlInsert = require('./insertXmlContent')
;

//might need to stream this though







function addPowerPlantInstance() {

    let ws = fs.createWriteStream(path.join(__dirname, 'out.owl'));
    let parentName = "system:TopLevelSystem";
    let content ='';
    let ppFileDir  = path.join(config.root, "powerplants");
    //get a list of all files to be added
     fs.readdir(ppFileDir, function (err, filenames) {


         function addcontent() {
             console.log(filenames.length);

               filenames = filenames.slice(0, 10);

             for(let ppName of filenames){
                 ws.write(getPPInstance(ppName)+"\n") ;
             }
         }
         xmlInsert().insertXML(worldNode,ws, parentName, addcontent,
             function () {//TODO:err handler

             }, function () {//end handler
                   console.log("done");
             } );
     })






    function getPPInstance(ppName) {
         ppName = ppName.replace(/&/g, "&amp;")
        ppName = ppName.replace(/.owl$/g, "" )
        var ppTemp = `   <system:hasSubsystem>
      <PowerPlant rdf:ID="Swanbank_B_Coal_Power_Plant_Australia">
        <system:hasConceptualRepresentation>
          <system:ConceptualModel rdf:ID="Swanbank_B_Coal_Power_Plant_Australia">
            <system:hasIRI rdf:datatype="http://www.w3.org/2001/XMLSchema#string">http://www.theworldavatar.com/Swanbank_B_Coal_Power_Plant_Australia.owl</system:hasIRI>
          </system:ConceptualModel>
        </system:hasConceptualRepresentation>
      </PowerPlant>
    </system:hasSubsystem>`;

        return ppTemp.replace(/Swanbank_B_Coal_Power_Plant_Australia/g, ppName);
    }

}

addPowerPlantInstance();