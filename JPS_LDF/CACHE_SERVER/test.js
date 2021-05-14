const express = require('express')
const app = express()
const port = 3001

app.get('/', (req, res) => {
	
	console.log('query is', req.query);
    res.send(req.query);
})

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
})