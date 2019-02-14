/**
get children list of a single file
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


module.exports = getChildrenSingleFile;