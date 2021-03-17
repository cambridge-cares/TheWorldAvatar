const express = require('express')
const app = express()
const port = 3000
const request = require('request');



app.get('/', (req, res) => {
   
   
   // TODO: implement Redis based caching here ... 
   
  query = req.query;
  console.log('the query is' , query);   
   

   
   
   
	var url = 'http://localhost:3001';

	request({url:url, qs:query}, function(err, response, body) {
	  if(err) { console.log(err); return; }
	  console.log("Get response: " + response.statusCode);
	  console.log('body');
	  
	  res.send(body); 
	 }); 
   
	
})

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
})