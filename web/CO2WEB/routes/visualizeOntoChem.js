
var topNode = require("../config").ontohemNode;
console.log("****************")
console.log(topNode)
let opts = {topnode:topNode,viewName:'visualEx',supQuery:
    
    
    `
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontochem: <https://como.cheng.cam.ac.uk/kb/ontochem.owl#>
    SELECT distinct  ?uri ?parent ?label
    WHERE {
 ?uri rdf:type ontochem:ReactionMechanism .}

  LIMIT 200
`};


var router = require("./routerFact/visualizeRouterFactExpand")(opts);

module.exports = router;
