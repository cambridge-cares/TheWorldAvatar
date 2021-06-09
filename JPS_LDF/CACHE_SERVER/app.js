const express = require('express')
const app = express()
const port = 3000
const request = require('request');
const actorInitHttp = require('@comunica/actor-init-http');




// =================== execute the request =======================
const util = require('util');
const exec = util.promisify(require('child_process').exec);
// ===============================================================

// =================== set up the redis server  ==================

const redis = require('redis');
const client = redis.createClient({
    host: 'localhost',
    port: 6379,
    password: '1234'
});

client.on('error', err => {
    console.log('Error ' + err);
});

// ========================= testing the redis ===========
client.set('foo', 'bar', (err, reply) => { // run a simple test with the redis first ... 
    if (err) throw err;
	console.log('setting foo bar');
    console.log(reply);

    client.get('foo', (err, reply) => {
        if (err) throw err;
		console.log('getting foo bar');
        console.log(reply);
    });
}); 
 
 
// ========================================================


// To make a request to the LDF server using actor-init-http 
 async function make_request(url) {
  const { stdout, stderr } = await exec('node node_modules//@comunica//actor-init-http//bin//run.js ' + url); 
  return stdout;
}

var cors = require('cors') // allow the server to make CORS requests/response 
app.use(cors())
 
app.get('/', (req, res) => {
   
	res.setHeader('Content-Type', 'application/trig')	// to set the server to return triples 
	const url = 'http://localhost:3001/?s=dffefe'; 
 
	(async () => {
		let result = await make_request(url)
		res.end(result) // ATTENTION: the instead of res.send, res end must be used in this case ... 
	})(); // make the async request from command line using comunica/actor-init-http 
    
 
	
})

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
})