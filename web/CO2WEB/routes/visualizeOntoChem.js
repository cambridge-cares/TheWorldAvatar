
var topNode = require("../config").ontochemNode;
console.log("****************")
console.log(topNode);
let opts = {topnode:topNode};

opts['supQuery'] = 
[{qStr :`
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> 
PREFIX ontochem: <https://como.cheng.cam.ac.uk/kb/ontochem.owl#> 
SELECT distinct ?uri ?parent ?label
WHERE { 
		 ?uri rdf:type ontochem:ReactionMechanism .

} LIMIT 50
`, level:1},
{qStr:`
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> 
PREFIX ontochem: <https://como.cheng.cam.ac.uk/kb/ontochem.owl#> 
SELECT distinct ?uri ?parent ?label
WHERE { 

	
    ?phase ontochem:containedIn ?parent .
	?uri ontochem:belongsToPhase ?phase .
	?uri ontochem:hasEquation ?label .
	

}  LIMIT 100
`, level:2}
,
{qStr:`
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> 
PREFIX ontochem: <https://como.cheng.cam.ac.uk/kb/ontochem.owl#> 
SELECT distinct ?uri ?parent ?label
WHERE { 

	
    ?parent ontochem:hasReactantSpecification ?reactantSpec .
	 ?reactantSpec ontochem:hasReactant ?uri .
	 ?uri rdfs:label ?label .


} LIMIT 2000
`, level:3}
,
{qStr:`
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> 
PREFIX ontochem: <https://como.cheng.cam.ac.uk/kb/ontochem.owl#> 
SELECT distinct ?uri ?parent ?label
WHERE { 

	 
     ?parent ontochem:hasProductSpecification ?productSpec .
	 ?productSpec ontochem:hasProduct ?uri .
	 ?uri rdfs:label ?label .
} LIMIT 2000

`,level:3}

]
;

var router = require("./routerFact/visualizeRouterFact")(opts);

module.exports = router;
