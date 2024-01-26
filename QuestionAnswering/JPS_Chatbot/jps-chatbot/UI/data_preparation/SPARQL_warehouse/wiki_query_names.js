const newEngine = require('@comunica/actor-init-sparql').newEngine;

const myEngine = newEngine();

fs = require('fs');

fs.readFile('D:/data/smiles2/SMILE_1', 'utf8' , (err, data) => {
  if (err) {
    console.error(err)
    return
  }
  console.log(data)
})

 
query_properties = `

SELECT DISTINCT ?p 
WHERE {
<http://www.wikidata.org/entity/Q161660> ?p ?o ;
} 

`
let counter = 0;

console.time('Execution time');
var query = query_properties;
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
				fs.writeFile('D://data/smiles2/SMILE_'+ counter/1000, JSON.stringify(tmp) , function (err) {
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


