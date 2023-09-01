/*******IMPORT**********/
var expect = require("chai").expect;
var mocha = require('mocha')

var describe = mocha.describe

var util = require("util");
var path = require("path");
var fs = require('fs')
var watcher = require("../agents/groupChangeWatcher")
var config = require("../config");
const bmsFolder = config.bmsFolder;
const testFile = path.join(bmsFolder, 'test.owl')

//Now need to manually modify the test file to trigger this, TODO
describe("Watcher", function () {
     /*main behavior**/
    it('when registers, inform registee when file changes : case : only filenames' , function (done) {


        this.timeout(300000);
        console.log("Watch over "+bmsFolder);

        //write to test file
        const testRegistees = ["dummy", "fool"];


        let count = 0;

        const expectResults = {uri: testFile, filename: "test.owl"}
        var mwatcher = watcher(bmsFolder, function (data, mobserver, callback) {
          //do expect test here
            expect(data).to.deep.equal(expectResults);
            expect(mobserver).to.equal(testRegistees[count])
            count++;
            if(count===2){
                done();
            }
            callback(null);
        });


        testRegistees.forEach(function (name) {
            mwatcher.registerAll( name, true);
        })


    });

    /*test some structural thing***********/
})

