const newEngine = require('@comunica/actor-init-sparql').newEngine;
const fs = require('fs');
const express = require('express');
const path = require('path');

const app = express();
const router = express.Router();

/*
router.get('/',function(req,res){
  res.sendFile(path.join(__dirname+'/index.html'));
  //__dirname : It will resolve to your project folder.
});

router.get('/query', function(req,res){
	
	 
	const myEngine = newEngine();
	var query = req.query.query;
	console.log('query', query);
	
	(async () => {
		const result = await myEngine.query(query, {
		  sources: ['http://localhost:8080/ldfserver/ontokin'],
		});

				result.bindingsStream.on('data', (binding) => {
					console.log(binding.get('?Equation').value);
					res.write(binding.get('?Equation').value)
				});
				
				
				result.bindingsStream.on('end', () => {
					console.log('============= query finished ==============');
					res.end('done')
		});

	})();
	  
	 
	
	// res.send(query);
}); */
console.time('test');

basic_query = `

PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  DISTINCT  ?reaction ?Equation  
WHERE  {	
	 	
  	?reaction ontokin:hasEquation ?Equation .
} 

`;



query = `
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  DISTINCT  ?reaction ?Equation ?MechanismName 
WHERE  {	
	?MechanismIRI rdfs:label ?MechanismName .
	?Phase ontokin:containedIn ?MechanismIRI .
 
	{SELECT DISTINCT ?reaction ?Phase ?Equation
	WHERE{		
	?reaction ontokin:belongsToPhase ?Phase .
 	?reaction ontokin:hasEquation ?Equation .
	}}
 
	} GROUP BY ?reaction ?Equation ?MechanismName 
`;



q1 = `
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  DISTINCT  ?reaction ?Equation ?ActivationEnergy  
WHERE  {	
 
	?ArrheniusCoefficient  ontokin:hasActivationEnergy ?ActivationEnergy .
	?reaction <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#hasArrheniusCoefficient> ?ArrheniusCoefficient .
 	?reaction ontokin:hasEquation ?Equation . 
	}  GROUP BY ?reaction ?Equation ?ArrheniusCoefficient ?ActivationEnergy LIMIT 1
`;


q2 = `
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  DISTINCT  ?Equation  
WHERE  {		 
 
 [
  <http://www.theworldavatar.com/kb/ontokin/56_53.owl#ChemicalReaction_631070468656898_13>,
  <http://www.theworldavatar.com/kb/ontokin/Andrae_2008.owl#ChemicalReaction_1749249908516854_88>
 ] 
 ontokin:hasEquation ?Equation .
	 
	}  

`; 


q3 = `
 
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?Species  ?LennardJonesWellDepth ?label  ?Unit
{
	
	 
     ?TransportModel ontokin:hasLennardJonesWellDepth ?LennardJonesWellDepth .
     ?Species ontokin:hasTransportModel ?TransportModel .
     ?Species rdfs:label ?label .
	 ?Species rdfs:label "O2" .
	 
	  OPTIONAL{
		?TransportModel ontokin:hasLennardJonesWellDepthUnits ?Unit .
     } 
 
 }  LIMIT 1
`;

q4 = `
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?Species ?label ?LennardJonesWellDepth ?Unit
{
	
	
	 ?TransportModel ontokin:hasPolarizability ?LennardJonesWellDepth .
     ?Species ontokin:hasTransportModel ?TransportModel .
     ?Species rdfs:label ?label .
	 ?Species rdfs:label "O2" .
	 
	  OPTIONAL{
		?TransportModel ontokin:hasPolarizabilityUnits ?Unit .
     } 
	
	 
}  LIMIT 1

`;
const myEngine = newEngine();
// var query = req.query.query;
query = q4;
console.log('query', query);

(async () => {
	var reactions = []
	const result = await myEngine.query(query, {
		sources: ['http://localhost:8080/ldfserver/ontokin'] , reactants: ['H', 'OH', 'M']
	 // sources: ['http://localhost:8080/ldfserver/ontokin'], reactants: ['H', 'O2']
	   // sources: ['http://localhost:3002'],
	});

			result.bindingsStream.on('data', (binding) => {
				
				//reactions.push(binding.get("?ActivationEnergy").value + '\n' + binding.get("?Equation").value )
				  reactions.push(binding.get("?Species").value)  
				  reactions.push(binding.get("?LennardJonesWellDepth").value)
				  reactions.push(binding.get("?Unit").value)
				  reactions.push(binding.get("?label").value)


			});
			 
			result.bindingsStream.on('end', () => {
				console.log('============= query finished ==============');
				console.log(reactions);
				console.timeEnd('test');
				 
	});

})();

			
function printMemory(){
	
	const mu = process.memoryUsage();
	const heapUsed  = mu['heapUsed'] / 1024 / 1024 / 1024;
	
	
		//console.log(`Total allocated       ${Math.round(mbNow * 100) / 100} GB`);
	 console.log(`Allocated since start ${Math.round((heapUsed -heapTotal) * 100) / 100} GB`);				
 }

/*
app.use('/', router);
app.listen(process.env.port || 3000);
console.log('Running at Port 3000');*/