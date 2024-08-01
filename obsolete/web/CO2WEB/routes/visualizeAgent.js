var topNode = require("../config").agentShowcaseNode;
var router = require("./routerFact/visualizeRouterFact")({topnode:topNode, viewName: 'visualAgent'});

module.exports = router;

