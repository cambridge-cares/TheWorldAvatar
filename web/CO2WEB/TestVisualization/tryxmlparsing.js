
//Try libxmljs lib

var libxmljs = require("libxmljs");
var fs = require("fs");

try {
    var file = fs.readFileSync("./C-301.owl");
    var xmlDoc = libxmljs.parseXml(file);
    var root = xmlDoc.root();




    function getGeoCoord(root) {
        if(!root){
            return null;
        }
        let x =  root.find("//owl:NamedIndividual[contains(@rdf:about, 'ValueOf_x_')]", {owl:'http://www.w3.org/2002/07/owl#', rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#"});
        console.log(x[0].text().trim());

        let y =  root.find("//owl:NamedIndividual[contains(@rdf:about, 'ValueOf_y_')]", {owl:'http://www.w3.org/2002/07/owl#', rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#resource"});

        console.log(y);
        return {x,y};
    }

    var result = getGeoCoord(root);
} catch (err){

    throw err;
}


