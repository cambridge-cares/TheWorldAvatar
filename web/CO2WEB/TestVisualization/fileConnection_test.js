/**
Test fileConnection module, which extracts all connections out of current grOWLs, starting from top node on disk
 */

/*******IMPORT**********/
var expect = require("chai").expect;
var util = require("util");
var rdfParser = require( "../util/rdfParser.js");
var request = require("request");
var path = require("path");
var connections = require("../util/fileConnection.js");


var config = require("../config");

var topNodeAddress = config.worldNode;


describe("extract connections", function () {
    it('find all connections of each file with no extra options', function (done) {
        this.timeout(3000);

        connections({topnode : topNodeAddress}, function(err, results){

            if(err){
                console.log(err);
                done(err);
            }
            console.log(JSON.stringify(results));
            expect(results).to.be.a('array');
            expect(results).to.have.length.above(0);
            for(let conn of results){
                expect(conn).to.have.property('source');
                expect(conn).to.have.property('target');
                expect(conn).to.have.property('level');
                console.log("find connections: "+ results.length);

            }

            done();

        })
    });

    it('find all connections of each file including imports with option:showImport', function (done) {
        this.timeout(300000);

        connections({topnode : topNodeAddress,showImport:true}, function(err, results){

            if(err){
                console.log(err);
                done(err);
            }
            console.log(JSON.stringify(results));
            expect(results).to.be.a('array');
            expect(results).to.have.length.above(0);
            console.log("find connections: "+ results.length);
            for(let conn of results){
                console.log(conn);
                expect(conn).to.have.property('source');
                expect(conn).to.have.property('target');
                //        expect(conn).to.have.property('level');

            }

            done();

        })
    });

    it('find only service type connection of each file with option:showServiceOnly', function (done) {
        this.timeout(6000);

        connections({topnode : topNodeAddress, showServiceOnly:true}, function(err, results){

            if(err){
                console.log(err);
                done(err);
            }
            console.log(JSON.stringify(results));
            expect(results).to.be.a('array');
            expect(results).to.have.length.above(0);
            console.log("find connections: "+ results.length);
            for(let conn of results){
                console.log(conn);
                expect(conn).to.have.property('source');
                expect(conn).to.have.property('target');
                expect(conn).to.not.have.property('level');
            }

            done();

        })
    });



});
