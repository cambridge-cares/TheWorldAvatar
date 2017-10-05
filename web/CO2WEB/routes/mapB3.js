/**
 * Created by Shaocong on 9/29/2017.
 */
const routerFact = require("./mapRouterFact"),
    getB3Coordi = require("../agents/GetB3Coordi");


var router = routerFact(getB3Coordi, {title:"BiodeselPlant3 Map", subtitle:"BiodeselPlant3 Map"}, "b3map");

module.exports = router;

