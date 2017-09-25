/**
 * Created by Shaocong on 9/25/2017.
 */
const xmlProcessor = require("./fileConnection")
    ,fs = require('fs')


function getChildrenSingleFile(uri, callback) {

    let loc = xmlProcessor.uriList2DiskLoc([uri])[0].diskLoc;

    console.log("loc : "+loc)
    fs.readFile(loc, function (err, file) {
        if(err){
            callback(err)
            return
        }
        let root = xmlProcessor.parseXMLFile(file);

        let children = xmlProcessor.getChildren(root);
        console.log(children);

        callback(null, children)
    })

}

//getChildrenSingleFile("http://www.theworldavatar.com/BMS/VAV-E7-11.owl", function () {});

module.exports = getChildrenSingleFile;