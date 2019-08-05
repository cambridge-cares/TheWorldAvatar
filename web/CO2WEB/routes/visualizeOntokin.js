
var topNode = require("../config").ontokinNode;
console.log("****************")
console.log(topNode)
let opts = {useSharp:true, topnode:topNode,viewName:'visualEx',supQuery:
    
    
    `
 PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX ontokin2: <http://theworldavatar.com/kb/ontokin/ontokin.owl#>

SELECT ?uri
    WHERE {{
    ?uri rdf:type ontokin:ReactionMechanism .
	} 
	}

`};


var router = require("./routerFact/visualizeRouterFactExpand")(opts);

module.exports = router;
/**
 */
