/**
Test fileConnection module, which extracts all connections out of current grOWLs, starting from top node on disk
 */

/*******IMPORT**********/
var expect = require("chai").expect;
var util = require("util");
var rdfParser = require( "../util/rdfParserObsolete.js");
var request = require("request");
var path = require("path");
var connections = require("../agents/fileConnection.js");


var config = require("../config");

var topNodeAddress = config.worldNode;


describe("extract connections", function () {
    it('find all connections of each file with no extra options', function (done) {
        this.timeout(3000);

        connections.readConnections({topnode : topNodeAddress}, function(err, results){

            if(err){
                console.log(err);
                done(err);
            }
            console.log(JSON.stringify(results));
            expect(results).to.be.a('object');
            expect(results).to.have.property('connections');
            expect(results).to.have.property('geoCoords');
            expect(results['connections']).to.have.length.above(0);
            expect(results['geoCoords']).to.have.length.above(0);

            for(let conn of results['connections']){
                expect(conn).to.have.property('source');
                expect(conn).to.have.property('target');
                expect(conn).to.have.property('level');
                console.log("find connections: "+ results['connections'].length);

            }
            for(let coord of results['geoCoords']){
                expect(coord).to.have.property('url');
                expect(coord).to.have.property('coord');
                expect(coord['coord']).to.have.property('x');
                expect(coord['coord']).to.have.property('y');

                console.log("find coordis: "+ results['geoCoords'].length);

            }
            done();

        })
    });

    it('find all connections of each file including imports with option:showImport', function (done) {
        this.timeout(300000);

        connections.readConnections({topnode : topNodeAddress,showImport:true}, function(err, results){

            if(err){
                console.log(err);
                done(err);
            }
            console.log(JSON.stringify(results));
            expect(results).to.be.a('object');
            expect(results).to.have.property('connections');
            expect(results).to.have.property('geoCoords');
            expect(results['connections']).to.have.length.above(0);
            expect(results['geoCoords']).to.have.length.above(0);

            for(let conn of results['connections']){
                console.log(conn);
                expect(conn).to.have.property('source');
                expect(conn).to.have.property('target');
                //        expect(conn).to.have.property('level');
                console.log("find connections: "+ results['connections'].length);

            }

            done();

        })
    });

    it('find only service type connection of each file with option:showServiceOnly', function (done) {
        this.timeout(6000);

        connections.readConnections({topnode : topNodeAddress, showServiceOnly:true}, function(err, results){

            if(err){
                console.log(err);
                done(err);
            }
            console.log(JSON.stringify(results));
            expect(results).to.be.a('object');
            expect(results).to.have.property('connections');
            expect(results).to.have.property('geoCoords');
            expect(results['connections']).to.have.length.above(0);
            expect(results['geoCoords']).to.have.length.above(0);

            for(let conn of results['connections']){
                console.log(conn);
                expect(conn).to.have.property('source');
                expect(conn).to.have.property('target');
                //        expect(conn).to.have.property('level');
                console.log("find connections: "+ results['connections'].length);

            }

            done();

        })
    });



});
