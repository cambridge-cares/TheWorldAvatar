/**
get children list of a single file
 */
const xmlProcessor = require("./fileConnection")
    ,fs = require('fs')

/***
async func to get a list of children(hasIri) iris of one single file
return : list of children iris
***/
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


module.exports = getChildrenSingleFile;