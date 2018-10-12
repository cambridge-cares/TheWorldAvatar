/**
 * Created by Shaocong on 9/4/2017.
 * A simple tool to add new Powerplant instance to theworld owlfile
 * This is for own use and not published online
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';


var config = require('../../config')
    ,path =require('path')
    ,fs = require('fs')
    ,worldNode = "C:/Users/Shaocong/WORK/webJPSGit/irp3-WebJPS-git/CO2WEB/testFiles/FakeParent.owl"
    ,root = config.root
    ,xmlInsert = require('../insertXmlContent')
;

//might need to stream this though







function addPowerPlantInstance() {

    let ws = fs.createWriteStream(path.join(__dirname, 'out.owl'));
    let parentName = "system:TopLevelSystem";
    let content ='';
    let ppFileDir  = path.join(config.root, "ppAl");
    //get a list of all files to be added
     fs.readdir(ppFileDir, function (err, filenames) {


         function addcontent() {
             logger.debug(filenames.length);

             for(let ppName of filenames){
                 ws.write(getPPInstance(ppName)+"\n") ;
             }
         }
         xmlInsert().insertXML(worldNode,ws, parentName, addcontent,
             function () {//TODO:err handler

             }, function () {//end handler
                   logger.debug("done");
             } );
     })







}

function getPPInstance(ppName) {
    ppName = ppName.replace(/&/g, "&amp;")
    ppName = ppName.replace(/.owl$/g, "" )
    var ppTemp = `   <system:hasSubsystem>
      <owl:NamedIndividual rdf:ID="Swanbank_B_Coal_Power_Plant_Australia">
        <system:hasConceptualRepresentation>
          <system:ConceptualModel rdf:ID="Swanbank_B_Coal_Power_Plant_Australia">
            <system:hasIRI rdf:datatype="http://www.w3.org/2001/XMLSchema#string">http://www.theworldavatar.com/ppAl/Swanbank_B_Coal_Power_Plant_Australia.owl</system:hasIRI>
          </system:ConceptualModel>
        </system:hasConceptualRepresentation>
      </owl:NamedIndividual>
    </system:hasSubsystem>`;
    
    return ppTemp.replace(/Swanbank_B_Coal_Power_Plant_Australia/g, ppName);
}
//addPowerPlantInstance();
