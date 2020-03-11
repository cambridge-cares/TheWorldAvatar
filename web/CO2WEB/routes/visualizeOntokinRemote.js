
var topNode = require("../config").ontokinRemoteNode;
console.log("****************")
console.log(topNode)
let opts = {useSharp:true, topnode:topNode,viewName:'visualizeExUpdate',supQuery:
    
    
    `
 PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>

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
/**
 */
