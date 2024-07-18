/**
 * Created by Shaocong on 9/4/2017.
 * Try sax module, copy a file with new child tag.
 */
var sax = require("sax"),
    strict = false, // set to false for html-mode
    parser = sax.parser(strict),
    fs = require('fs');



parser.onerror = function (e) {
    // an error happened.
};
parser.ontext = function (t) {
    // got some text.  t is the string of text.
};
parser.onopentag = function (node) {
    // opened a tag.  node has "name" and "attributes"
    console.log(JSON.stringify(node))
};
parser.onattribute = function (attr) {
    // an attribute.  attr has "name" and "value"
};
parser.onend = function () {
    // parser stream is done, and ready to have more stuff written to it.
};

// pipe is supported, and it's readable/writable
// same chunks coming in also go out.
fs.createReadStream("file.xml")
    .pipe(parser)
   // .pipe(fs.createWriteStream("file-copy.xml"))