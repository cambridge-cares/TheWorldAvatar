/**
 * Created by Shaocong on 9/4/2017.
 */
var fs        = require('fs')
    , path      = require('path')
    , XmlStream = require('xml-stream')
;

// Create a file stream and pass it to XmlStream
function setup(encoding) {
    var stream = fs.createReadStream(path.join(__dirname, 'TheWorld.owl'));
    var ws = fs.createWriteStream(path.join(__dirname, 'out.owl'))
    var xml = new XmlStream(stream);
    xml.on('data', function(node) {
        //console.log(node);
        //console.log("____________________")
        ws.write(node)
    });
    xml.on('endElement:system:TopLevelSystem', function(node) {
    //write whatever you want to add to this level

    });
    xml.on('error', function(message) {
        console.log('Parsing as ' + (encoding || 'auto') + ' failed: ' + message);
    });
    return xml;
}

var xml = setup('utf8');       // Parse as UTF-8
