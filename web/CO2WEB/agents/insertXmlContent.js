/**
 * Utility function, not published on map:
 * stream-inserting some content into a certain parent node in xml
 *
 */
var log4js = require('log4js');
var logger = log4js.getLogger();
logger.level = 'debug';

var fs        = require('fs')
    , XmlStream = require('xml-stream')
;


function insertXMLer() {
    function insertXML(inLoc, outStream, parent, content, errlistener, endlistener) {

        endlistener = endlistener || function () {

            };
        errlistener = endlistener || function () {

            };
        var stream = fs.createReadStream(inLoc);
        //var ws = fs.createWriteStream(outLoc)
        var xmls = new XmlStream(stream);
        var called = false;

        xmls.on('data', function(node) {
            logger.debug("get data")
            outStream.write(node)//copy content to
        });
        xmls.on('endElement:'+parent, function(node) {
            called = true;
            //write whatever you want to add to this level
            if(isFunction(content)){
                content(outStream);
            } else if(typeof content === 'string'){
                outStream.write(content);
            } else{
                throw new Error("content of wrong type: " + (typeof  content));
            }
        });
        xmls.on('error', errlistener);
        xmls.on('end',function () {
            if(!called){
                throw new Error("parent node: "+parent+" is not found in file");
            }
            endlistener();
        });

    }
    function isFunction(functionToCheck) {
        var getType = {};
        return functionToCheck && getType.toString.call(functionToCheck) === '[object Function]';
    }
  return {insertXML};
}


module.exports = insertXMLer;


