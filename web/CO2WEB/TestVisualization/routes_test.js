/***
 * test all routes defined by server
 */
/*******IMPORT**********/
var expect = require("chai").expect;
var util = require("util");
var rdfParser = require( "../util/rdfParser.js");
var request = require("request");
var path = require("path");


var config = require("../config");


var baseURL = "http://localhost:" + config.port;


var app = require("../app");


before(function () {//open server before all tests
    app.listen(3000, function () {
        console.log('Server listening on port 3000');
    });
});


describe('get /visualizeWorld', function () {

    it('returns status 200',function (done) {
        this.timeout(30000);
        let url = baseURL + "/visualizeWorld";
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

describe('get /visualizeBms', function () {

    it('returns status 200',function (done) {
        this.timeout(30000);

        let url = baseURL + "/visualizeBms";
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
describe('get /visualizeJurong', function () {

    it('returns status 200',function (done) {
        let url = baseURL + "/visualizeJurong";
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
describe('get /visualizeSemakau', function () {

    it('returns status 200',function (done) {
        let url = baseURL + "/visualizeSemakau";
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


describe('get /visualizeWorld/includeImport', function(){

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

describe('get /bmsplot ', function () {
    it('returns status 200',function (done) {
        let url = baseURL + "/bmsplot";
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

