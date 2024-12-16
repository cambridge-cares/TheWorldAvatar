
const newEngine = require('@comunica/actor-init-sparql').newEngine;

const myEngine = newEngine();

fs = require('fs');
// fs.writeFile('ontokin', data, [encoding], [callback])


ontokin_obj = {'equations': [], 'species': []};
ontocompchem_obj = {'species': []};


ontokin_get_equation = `
		PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX ontokin:
        <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
        PREFIX reaction:<http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
		
		SELECT  DISTINCT   ?Equation 
        WHERE  {
			
			?reaction ontokin:hasEquation ?Equation .
        }  LIMIT 500
 
`;



ontokin_get_species = `

PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?name 
{
  ?Species rdfs:label ?name .
  ?Species ontokin:hasTransportModel ?TransportModel . 
}  LIMIT 500
`
 
ontocompchem = `


PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
PREFIX gc: <http://purl.org/gc/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
SELECT DISTINCT ?name  

WHERE  {
?molecule_property gc:hasName ?name . 
}  LIMIT 500
`


myEngine.invalidateHttpCache();


console.time('Execution time');

 
	


(async () => {
	
	var query = ontocompchem;
	let rst = await make_query(query, 'ontocompchem');
	ontocompchem_obj['species'] = rst; 
	
	
	query = ontokin_get_species;
	rst = await make_query(query, 'ontokin');
	ontokin_obj['species'] = rst; 
	
	query = ontokin_get_equation;
	rst = await make_query(query, 'ontokin');
	ontokin_obj['equations'] = rst; 
	
	fs.writeFile('ontocompchem_species', JSON.stringify(ontocompchem_obj), function (err) {
		if (err) return console.log(err);
			console.log('Wrote to ontokin_species_and_equations');
		});
		
	fs.writeFile('ontokin_species_and_equations', JSON.stringify(ontokin_obj), function (err) {
		if (err) return console.log(err);
			console.log('Wrote to ontocompchem_species');
		});	
			
	
})();

 
async function make_query(query, ontology){
	
	
		const result = await myEngine.query(query, {
		 // sources: ['http://localhost:8080/ldfserver/ontokin'], products:["placeholder"], reactants:["placeholder"]  
		 sources: ['http://localhost:8080/ldfserver/' + ontology], products:['placeholder'].sort(), reactants:["placeholder"].sort()
		});


		const bindings = await result.bindings();
		 
		let full_result = [];
		for (let binding of bindings){
			let row = parse_bindings(binding);
			full_result.push(row);
		}
		// console.log(JSON.stringify(full_result));
		console.timeEnd('Execution time');
		return JSON.stringify(full_result);
	
}

function parse_bindings(binding) {
	let _rst = [];
    let _obj = binding.toObject();
	let _keys = Object.keys(_obj);
	let	_row = [];

	for (let key of _keys){
		value = _obj[key].value;
		
		return value
		key =  key.replace('?', '') ;
		_row.push(value);
	}
	
	// return _row
 }


