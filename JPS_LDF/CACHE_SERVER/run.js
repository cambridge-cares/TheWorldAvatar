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
 
        }  LIMIT 10
 
`;

  

console.time('Execution time');
var query = basic_query;


	(async () => {
		const result = await myEngine.query(query, {
		  sources: ['http://localhost:8080/ldfserver/ontokin'], products:["OH"], reactants:["H"]
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


