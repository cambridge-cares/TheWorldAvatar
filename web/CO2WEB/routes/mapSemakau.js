
const routerFact = require("./mapRouterFact"),
      getSemakauCoordi = require("../agents/GetSemakauCoordi");
var express= require('express')
var router = express.Router()

    var router = routerFact(router, getSemakauCoordi, {title:"Semakau Map", subtitle:"Semakau Map"}, "semakauMap");

    module.exports = router;

