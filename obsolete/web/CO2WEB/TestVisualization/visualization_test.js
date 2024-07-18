
/*******IMPORT**********/
var expect = require("chai").expect;
var util = require("util");
var rdfParser = require( "../util/rdfParser.js");
var request = require("request");
var path = require("path");
var connections = require("../util/fileConnection.js");


var config = require("../config");

var opts = {
    fileUrl: config.root + "/JurongIsland.owl",
    uri: 'http://www.theworldavatar.com/JurongIsland.owl'
};
var baseURL = "http://localhost:" + config.port;




describe('RDFParser: ', function () {

    it('could find the value of property of an indivdiual in a specific file', function () {


        let node = 'http://www.jparksimulator.com/JurongIsland.owl#V_CO2_Jurong';
        let property = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue";
        let result = rdfParser(opts).search(node, property);
      console.log(util.inspect(result));

        expect(result.value).to.equal('1500.0');

    })


});





describe("extract connections", function () {
    it('find all connections of each file with no extra options', function (done) {
  this.timeout(3000);

        connections({}, function(err, results){

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
  this.timeout(3000);

        connections({showImport:true}, function(err, results){

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
  this.timeout(3000);

        connections({showServiceOnly:true}, function(err, results){

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


describe('get /visualize', function () {

    it('returns status 200',function (done) {
        let url = baseURL + "/visualize";
        console.log("request "+ url);

        request.get({url : url}, function(error, response, body) {
            if(error){
                console.log(error);
                done(error);
            }
            expect(response.statusCode).to.equal(200);


            done();
        })

    })

});

describe('get /visualize/includeImport', function(){

    it('returns json', function(){
        let url = baseURL + "/visualize/includeImport";
        console.log("request "+ url);

        request.get({url : url}, function(error, response, body) {
            if(error){
                console.log(error);
                done(error);

            }
            expect(response.statusCode).to.equal(200);
            let results = JSON.parse(body);
            expect(results).to.be.a('array');
            expect(results).to.have.length.above(0);
            for(let conn of results){
                expect(conn).to.have.property('source');
                expect(conn).to.have.property('target');

            }

            done();
        })

    })
});

describe('get /JurongIsland.owl/showCO2', function(){
    it('returns status 200',function (done) {
        let url = baseURL + "/JurongIsland.owl/showCO2";
        console.log("request "+ url);

        request.get({url : url}, function(error, response, body) {
            if(error){
                console.log(error);
                done(error);
            }
            expect(response.statusCode).to.equal(200);


            done();
        })

    })

});
