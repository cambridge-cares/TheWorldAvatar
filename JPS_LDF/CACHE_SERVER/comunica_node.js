const newEngine = require('@comunica/actor-init-sparql').newEngine;
const fs = require('fs');
const express = require('express');
const path = require('path');
const app = express();
const router = express.Router();


router.get('/query', function(req,res){
	 
	console.time('Execution time');
	const myEngine = newEngine();
	
	myEngine.invalidateHttpCache();

	// console.log('query', req.query);
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
		res.send(full_result);
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


app.use('/', router);
app.listen(process.env.port || 3000);
console.log('Running at Port 3000');