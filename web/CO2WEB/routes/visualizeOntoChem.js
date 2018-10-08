
var topNode = require("../config").ontochemNode;
console.log("****************")
console.log(topNode);
var router = require("./routerFact/visualizeRouterFact")(topNode);

module.exports = router;
