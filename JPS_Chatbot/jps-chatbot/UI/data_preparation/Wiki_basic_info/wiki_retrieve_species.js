const newEngine = require('@comunica/actor-init-sparql').newEngine;

const myEngine = newEngine();

fs = require('fs');




query_smiles = `

SELECT ?species ?SMILES 
WHERE {
?species <http://www.wikidata.org/prop/direct/P233> ?SMILES ;

} 

`
let counter = 0;

console.time('Execution time');
var query = query_smiles;
tmp = [];
	(async () => {
		const result = await myEngine.query(query, {
		 sources: ['https://query.wikidata.org/bigdata/ldf'] 
		});

		result.bindingsStream.on('data', (binding) => {
			r = parse_bindings(binding);
			tmp.push(r);
			console.log(r)
			counter++
			if (counter % 1000 == 0){
				fs.writeFile('./smiles/SMILE_'+ counter/1000, JSON.stringify(tmp) , function (err) {
				if (err) return console.log(err);
				  console.log('Hello World > helloworld.txt');
				});
				
				tmp = [];
			}
			console.log(counter)
		});	
		
		
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


