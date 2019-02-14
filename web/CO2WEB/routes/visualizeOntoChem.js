
var topNode = require("../config").ontochemNode;
console.log("****************")
console.log(topNode);
let opts = {topnode:topNode};
opts['supQuery'] = 
`
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> 
PREFIX ontochem: <https://como.cheng.cam.ac.uk/kb/ontochem.owl#> 
SELECT distinct ?uri ?parent
WHERE { 
		 {?uri rdf:type ontochem:ReactionMechanism .}
UNION
	{
    ?phase ontochem:containedIn ?parent .
	?chemicalReaction ontochem:belongsToPhase ?phase .
	?chemicalReaction ontochem:equation ?uri .
	}
	UNION
	{
    ?parent ontochem:hasReactantSpecification ?reactantSpec .
	 ?reactantSpec ontochem:hasReactant ?reactantSpecies .
	 ?reactantSpecies rdfs:label ?uri .
	 }
	 UNION
	 	 { 
     ?parent ontochem:hasProductSpecification ?productSpec .
	 ?productSpec ontochem:hasProduct ?productSpecies .
	 ?productSpecies rdfs:label ?uri .
    }
}
`;

var router = require("./routerFact/visualizeRouterFact")(opts);

module.exports = router;
