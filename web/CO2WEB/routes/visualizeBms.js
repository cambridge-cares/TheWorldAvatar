
var topNode = require("../config").bmsNode;
console.log("****************")
console.log(topNode);
var router = require("./routerFact/visualizeRouterFactCached")(topNode);

module.exports = router;
