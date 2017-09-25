/**
 * Created by Shaocong on 9/21/2017.
 */
const routerFact = require("./mapRouterFact"),
      getSemakauCoordi = require("../agents/GetSemakauCoordi");


    var router = routerFact(getSemakauCoordi, {title:"Semakau Map", subtitle:"Semakau Map"}, "semakauMap");

    module.exports = router;

