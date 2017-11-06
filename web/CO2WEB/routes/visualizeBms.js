
var topNode = require("../config").bmsNode;
console.log("****************")
console.log(topNode);
var router = require("./visualizeRouterFact")(topNode);

module.exports = router;
