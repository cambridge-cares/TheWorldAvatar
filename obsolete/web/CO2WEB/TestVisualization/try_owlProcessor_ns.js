/**
 * Created by Shaocong on 9/7/2017.
 */
var owlProcessor = require('../agents/fileConnection');

var fs = require('fs')

let testFile = "C:/Users/Shaocong/WORK/webJPSGit/irp3-WebJPS-git/CO2WEB/testFiles/E-301.owl"


let file = fs.readFileSync(testFile);

 let root = owlProcessor.parseXMLFile(file);


 let nsList = owlProcessor.getNSList(root);