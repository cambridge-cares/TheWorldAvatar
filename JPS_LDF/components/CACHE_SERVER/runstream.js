var crypto = require('crypto');
var sha = crypto.createHash('sha1');
const fs = require('fs');
const express = require('express');
const path = require('path');
const app = express();
const router = express.Router();
const http = require('http')
const cors = require('cors')
const redis = require('./redis-interface');

// TODO: replace this with the redis server to be more robust 
result_dictionary = {};
status_dictionary = {};
	
	
const newEngine = require('@comunica/actor-init-sparql').newEngine;
const myEngine = newEngine();
myEngine.invalidateHttpCache();

router.get('/stream/loadmore', function(req, res){
	let data = req.query;
	parameter_hash = data.hash;
	console.log('hash from load more', parameter_hash);
	if (parameter_hash in result_dictionary){ // the key exist, then check the dictionary and return the result 
		
		query_status = status_dictionary[parameter_hash];
		if (query_status){ // the status is true, means all the results are retreived. 
			rst = {'hash': parameter_hash, 'status': 'complete', 'result': result_dictionary[parameter_hash]}
			res.send(JSON.stringify(rst));		
		}else{// the status is false, means the query is still going on 
			rst = {'hash': parameter_hash, 'status': 'ongoing', 'result': result_dictionary[parameter_hash]}
			res.send(JSON.stringify(rst));
		}
	}
	else{
		rst = {'status': 'invalid', 'result': []};
		res.send(JSON.stringify(rst));	
	}
	
	
});


router.get('/query', function(req, res){
	myEngine.invalidateHttpCache();
	let data = req.query;
	console.log('from query', data);
	let ontology = '';
	if (data.ontology){
		ontology = data.ontology; 
	}
	
	parameter_hash = data.hash;
	query = data.query; 	

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
	
	// query different sub-endpoints for different ontology 
	let parameters = {};
	if (ontology === 'ontokin'){
		console.log('querying ontokin');
		parameters.sources = ['http://localhost:8080/ldfserver/ontokin'];
		parameters.products =  products;
		parameters.reactants = reactants;
	}else{
		console.log('querying ontocompchem');
		parameters.sources = ['http://localhost:8080/ldfserver/ontocompchem'];
	}
	
	console.log('parameters', parameters);


	// the key already exists in the result dictionary, so directly return the results and the status 
	if (parameter_hash in result_dictionary){

				if (status_dictionary[parameter_hash]){
					query_status = 'complete';
				}else{
					query_status = 'ongoing';
				}
		
		rst = {'hash': parameter_hash, 'status': query_status, 'result': result_dictionary[parameter_hash]}
		res.send(JSON.stringify(rst));	
		
	}
	else{ // the result is not there,do the query
		result_dictionary[parameter_hash] = [];
		status_dictionary[parameter_hash] = false;
		
		// ================ redis initiation =========================
		// redis.set_value(parameter_hash, JSON.stringify([]))
		 
    	(async () => {
		const result = await myEngine.query(query,parameters);
		
		result.bindingsStream.on('data', (binding) => {
			r = parse_bindings(binding);
			result_dictionary[parameter_hash].push(r);
			// redis.set_value(parameter_hash, JSON.stringify([])) // set the value in redis
			
			
			if (result_dictionary[parameter_hash].length == 10){
				
				if (status_dictionary[parameter_hash]){
					query_status = 'complete';
				}else{
					query_status = 'ongoing';
				}
				rst = {'hash': parameter_hash, 'status': query_status, 'result': result_dictionary[parameter_hash]}
				res.send(JSON.stringify(rst));	
			}
			
		});	
		
		
		// on end is triggered when all the results are returned 		
		result.bindingsStream.on('end', () => {
			
			status_dictionary[parameter_hash] = true;
			console.log('========== the end ============');  
			if (result_dictionary[parameter_hash].length < 10){
				console.log('less than 10 results in KG');
				rst = {'hash': parameter_hash, 'status': 'complete', 'result': result_dictionary[parameter_hash]}
				res.send(JSON.stringify(rst));	
			}
			else{
				status_dictionary[parameter_hash] = true;
			}
		});
		})();
		
		
	}
	
});


	
app.use('/', cors(), router, (error, req, res, next) => {

 	req.setTimeout(25 * 1000, function(){
        // call back function is called when request timed out.
    });
 res.status(500).send(error);
});

app.listen(process.env.port || 3000);
console.log('Running at Port 3000');	
	
	


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