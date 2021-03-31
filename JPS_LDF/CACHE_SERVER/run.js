const newEngine = require('@comunica/actor-init-sparql').newEngine;

const myEngine = newEngine();

basic_query = `
		PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX ontokin:
        <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
        PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
		
		SELECT  DISTINCT   ?Equation ?MechanismName
        WHERE  {
			
			
         ?Phase ontokin:containedIn ?MechanismIRI .
		 ?MechanismIRI rdfs:label ?MechanismName .

		 {
			 SELECT DISTINCT ?Phase ?Equation
			 WHERE {
				 
				 ?reaction ontokin:hasEquation ?Equation .
				 ?reaction ontokin:belongsToPhase ?Phase .
			 }
			 
		 } 
 
        }  
 
`;


test_query =`


PREFIX rdf:      <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:     <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:  <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  DISTINCT  ?reaction ?Equation  
WHERE  {	
  	?reaction ontokin:hasEquation ?Equation .
    # FILTER regex(str(?Equation), ".*=] %s .*| .*=].* %s$")
    # FILTER regex(str(?Equation), ".*=] %s .*| .*=].* %s$")
}  

`


test_query_reaction_rate = `
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  DISTINCT  ?reaction ?Equation ?ActivationEnergy  
WHERE  {	
 
 		 ?ArrheniusCoefficient 	ontokin:hasActivationEnergy ?ActivationEnergy ;
								ontokin:hasActivationEnergyUnits  ?ActivationEnergyUnits  ;
								ontokin:hasPreExponentialFactor ?PreExponentialFactor ;
								ontokin:hasPreExponentialFactorUnits ?PreExponentialFactorUnits ;
								ontokin:hasTemperatureExponent ?TemperatureExponent ;
								ontokin:hasTemperatureExponentUnits ?TemperatureExponentUnits .
 
	{
	SELECT ?reaction ?Equation ?ArrheniusCoefficient
	WHERE {
			?reaction <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#hasArrheniusCoefficient> ?ArrheniusCoefficient .
			?reaction ontokin:hasEquation ?Equation . 
		}
	}
	

	}   LIMIT 1


`;


test_is_reversible = `
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:
<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  DISTINCT  ?Equation ?isReversible
WHERE  {		 
 
			?reaction ontokin:isReversible ?isReversible .
			?reaction ontokin:hasEquation ?Equation .
	 
	}  LIMIT 1

`;
  	myEngine.invalidateHttpCache();


console.time('Execution time');
var query = test_query;

	(async () => {
		const result = await myEngine.query(query, {
		    //sources: ['http://localhost:8080/ldfserver/ontokin'], products:["OH"], reactants:["H"] ,"NC3H7"
		 sources: ['http://localhost:8080/ldfserver/ontokin'], products:['CO','CH3CHO'].sort(), reactants:["placeholder"].sort()
		});


		const bindings = await result.bindings();
		 
		let full_result = [];
		for (let binding of bindings){
			let row = parse_bindings(binding);
			full_result.push(row);
		}
		console.log(JSON.stringify(full_result));
		console.timeEnd('Execution time');
		
		
	})();


function parse_bindings(binding) {
	let _rst = [];
    let _obj = binding.toObject();
	let _keys = Object.keys(_obj);
	let	_row = {};

	for (let key of _keys){
		value = _obj[key].value;
		key =  key.replace('?', '') ;
		_row[key] = value;
	}
	
	return _row
 }


