/**
 * Created by Shaocong on 9/6/2017.
 */
var libxml = require("libxmljs");
const util = require("util")
var xml2 =  `
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:terms="http://purl.org/dc/terms/" xmlns:swrlb="http://www.w3.org/2003/11/swrlb#" xmlns:xsp="http://www.owl-ontologies.com/2005/08/07/xsp.owl#" xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:protege="http://protege.stanford.edu/plugins/owl/protege#" xmlns:swrl="http://www.w3.org/2003/11/swrl#" xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" xmlns="http://www.theworldavatar.com/TheWorld.owl#" xmlns:system="http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#" xmlns:xsd="http://www.w3.org/2001/XMLSchema#">

     <system:hasSubsystem>
      <PowerPlant rdf:ID="0es_Coal_Plant_OH_USA">
        <system:hasConceptualRepresentation>
          <system:ConceptualModel rdf:ID="0es_Coal_Plant_OH_USA">
            <system:hasIRI rdf:datatype="http://www.w3.org/2001/XMLSchema#string"
            >http://www.theworldavatar.com/0es_Coal_Plant_OH_USA.owl</system:hasIRI>
          </system:ConceptualModel>
        </system:hasConceptualRepresentation>
      </PowerPlant>
    </system:hasSubsystem>
         <system:hasSubsystem>
      <PowerPlant rdf:ID="0es_Coal_Plant_OH_USA">
        <system:hasConceptualRepresentation>
          <system:ConceptualModel rdf:ID="0es_Coal_Plant_OH_USA">
            <system:hasIRI rdf:datatype="http://www.w3.org/2001/XMLSchema#string"
            >http://www.theworldavatar.com/0es_Coal_Plant_OH_USA.owl</system:hasIRI>
          </system:ConceptualModel>
        </system:hasConceptualRepresentation>
      </PowerPlant>
    </system:hasSubsystem>
       <system:hasSubsystem>
      <PowerPlant rdf:ID="1515_S_Caron_Road_Gas_Plant_IL_USA">
        <system:hasConceptualRepresentation>
          <system:ConceptualModel rdf:ID="1515_S_Caron_Road_Gas_Plant_IL_USA">
            <system:hasIRI rdf:datatype="http://www.w3.org/2001/XMLSchema#string"
            >http://www.theworldavatar.com/1515_S_Caron_Road_Gas_Plant_IL_USA.owl</system:hasIRI>
          </system:ConceptualModel>
        </system:hasConceptualRepresentation>
      </PowerPlant>
    </system:hasSubsystem>
</rdf:RDF>
`;
var fs = require('fs')
var config = require("../config");
var xml = fs.readFileSync(config.worldNode);

try{
    var xmlDoc = libxml.parseXmlString(xml);

// xpath queries
//    xmlns="http://www.theworldavatar.com/TheWorld.owl#"

    var gchild = xmlDoc.find('//f:PowerPlant',{'f': "http://www.theworldavatar.com/TheWorld.owl#"});
    console.log(gchild.length)
    console.log(util.inspect(gchild[0]))

    gchild.forEach(function (item) {
        //console.log(item.text());  // prints "grandchild content"

    })

}catch (err){
    throw err
}


