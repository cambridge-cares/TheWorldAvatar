/**
 * Created by Shaocong on 9/4/2017.
 */
/*******IMPORT**********/
var expect = require("chai").expect;
var util = require("util");
var rdfParser = require( "../agents/rdfParser.js");
var request = require("request");
var path = require("path");
var insertXml = require("../agents/insertXmlContent"),
    process = require("process");

var config = require("../config");
var MemoryStream = require('memorystream');
var memStream = MemoryStream.createWriteStream();

function trimSpace(str) {
    return  str.replace(/(\r\n|\n|\r|\s)/gm,"");
}

describe('insertXmlContent: ', function () {

    it('could insert content under a specific parent node in an xml file', function (done) {


        var inLoc = path.join(__dirname, "file.xml")
        var ws = process.stdout;
        insertXml().insertXML(inLoc, memStream, "root","<hello>world2</hello>\n",function () {
            
        }, function () {
            expect(       trimSpace(memStream.toString())
            ).to.equal(trimSpace(result));
            done();
        } )

        var result = `<root>
    <hello>world</hello>
    <hello>world2</hello>
</root>`



    });

    it('accept content to be a function', function (done) {
        var content = function (ws) {
            ws.write("<hello>world2</hello>\n")
        }
        var inLoc = path.join(__dirname, "file.xml")
        insertXml().insertXML(inLoc, memStream, "root", content,function () {

        }, function () {
            expect(       trimSpace(memStream.toString())
            ).to.equal(trimSpace(result));
            done();
        } )

        var result = `<root>
    <hello>world</hello>
    <hello>world2</hello>
</root>`
    });
    it('throw err is content is of any other type' ,function (done) {
        var inLoc = path.join(__dirname, "file.xml")
        var content = 1
        insertXml().insertXML(inLoc, memStream, "root", content,function () {

        }, function () {

        } )


    });
    it('throws err if can not find the parent node',function (done) {
        var content = function (ws) {
            ws.write("<hello>world2</hello>\n")
        }
        var inLoc = path.join(__dirname, "file.xml")
        insertXml().insertXML(inLoc, memStream, "panda", content,function () {

        }, function () {
            console.log(memStream.toString())
            expect(       trimSpace(memStream.toString())
            ).to.equal(trimSpace(result));
            done();
        } )
    })



});
