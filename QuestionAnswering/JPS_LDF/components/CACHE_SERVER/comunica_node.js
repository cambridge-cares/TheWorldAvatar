const newEngine = require('@comunica/actor-init-sparql').newEngine;
const fs = require('fs');
const express = require('express');
const path = require('path');
const app = express();
const router = express.Router();
const http = require('http')


router.get('/test', function(req,res){

const options = {
  hostname: 'localhost',
  port: 8080,
  path: '/ldfserver/ontokin',
  method: 'GET'
}

const request = http.request(options, response => {
  console.log(`statusCode: ${response.statusCode}`)

  response.on('data', d => {
    res.send(d)
  })
})

request.on('error', error => {
    res.send("Houston, we have a problem")
})

request.end()

});


router.get('/ontocompchem/query', function(req, res){
	console.time('Execution time ontocompchem');
	
	
	let data = req.query;
	const myEngine = newEngine();
 	let query = data.query;

		(async () => {
		const result = await myEngine.query(query, {
		     sources: ['http://localhost:8080/ldfserver/ontocompchem']
		});
	

		const bindings = await result.bindings();
		 
		let full_result = [];
		for (let binding of bindings){
			let row = parse_bindings(binding);
			full_result.push(row);
		}
		full_result = JSON.stringify(full_result);
		console.timeEnd('Execution time ontocompchem');
		res.status(200).send(full_result);
	})(); 
	
	
});




router.get('/query', function(req,res){
	 
	console.time('Execution time');
	const myEngine = newEngine();
	
	myEngine.invalidateHttpCache();

	let parameters  = {sources: ['http://localhost:8080/ldfserver/ontokin']};
	let data = req.query;
	
	
	let products = []
	let reactants = []
	if(data.products){
		products = JSON.parse(data.products);
		if (products.length === 0){
			products = ["placeholder"]
		}
		else{
			products =  Array.from(new Set(products.sort()));
		}
	}

	if(data.reactants){
		reactants = JSON.parse(data.reactants);
		if (reactants.length === 0){
			reactants = ["placeholder"]
		}
		else{
			reactants =  Array.from(new Set(reactants.sort()));
		}
	}	
	
	let query = data.query;
	(async () => {
		const result = await myEngine.query(query, {
		     sources: ['http://localhost:8080/ldfserver/ontokin'], products: products, reactants: reactants 
		});
	

		const bindings = await result.bindings();
		 
		let full_result = [];
		for (let binding of bindings){
			let row = parse_bindings(binding);
			full_result.push(row);
		}
		full_result = JSON.stringify(full_result);
		console.timeEnd('Execution time');
		res.status(200).send(full_result);
	})(); 
})



// make the result in the format that Marie's front end accepts 
// array of rows
// if the results is empty, return "Nothing"
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

	
 	
function printMemory(){
	
	const mu = process.memoryUsage();
	const heapUsed  = mu['heapUsed'] / 1024 / 1024 / 1024;
	
	
		//console.log(`Total allocated       ${Math.round(mbNow * 100) / 100} GB`);
	 console.log(`Allocated since start ${Math.round((heapUsed -heapTotal) * 100) / 100} GB`);				
 }
 
app.use('/', router, (error, req, res, next) => {

 	req.setTimeout(25 * 1000, function(){
        // call back function is called when request timed out.
    });
 res.status(500).send("Something Broke!");
});

app.listen(process.env.port || 3000);
console.log('Running at Port 3000');