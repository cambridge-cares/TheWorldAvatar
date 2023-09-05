/***
 * test all routes defined by server
 */
/*******IMPORT**********/
var expect = require("chai").expect;
var util = require("util");
var rdfParser = require( "../agents/rdfParserObsolete.js");
var request = require("request");
var path = require("path");


var config = require("../config");


var baseURL = "http://localhost:" + config.port;


var app = require("../app");



describe('get /JurongIsland.owl/showCO2', function () {

    it('returns status 200',function (done) {
        this.timeout(30000);
        let url = baseURL + "/JurongIsland.owl/showCO2";
        console.log("request "+ url);

        request.get({url : url}, function(error, response, body) {
            if(error){
                console.log(error);
                done(error);
            }
            console.log(body)
            expect(response.statusCode).to.equal(200);


            done();
        })

    })

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

describe('get /PowerPlantCO2 ', function () {
    it('returns status 200',function (done) {
        this.timeout(3000000);

        let url = baseURL + "/PowerPlantCO2";
        console.log("request "+ url);

        request.get({url : url}, function(error, response, body) {
            if(error){
                console.log(error);
                done(error);
            }
            console.log(body)
           // expect(response.statusCode).to.equal(200);


            done();
        })

    })
});

describe('get /listbycountry ', function () {
    it('returns status 200',function (done) {
        this.timeout(3000000);

        let url = baseURL + "/PowerPlantCO2/listbycountry";
        console.log("request "+ url);

        request({method:'POST', url : url, headers:{ "content-type": "application/json"},body:JSON.stringify({"country":"Slovakia"})}, function(error, response, body) {
            if(error){
                console.log(error);
                done(error);
            }
            console.log(body);

            expect(response.statusCode).to.equal(200);


            done();
        })

    })
});

describe('get /convertion ', function () {
    it('returns status 200',function (done) {
        this.timeout(3000000);

        let url = baseURL + "/PowerPlantCO2/convertion";
        console.log("request "+ url);

        request({method:'POST', url : url, headers:{ "content-type": "application/json"},body:JSON.stringify({"country":"Slovakia", "percent":50})}, function(error, response, body) {
            if(error){
                console.log(error);
                done(error);
            }
            console.log(body)
            expect(response.statusCode).to.equal(200);


            done();
        })

    })
});

describe('get /getAttrList ', function () {
    it('returns status 200',function (done) {
        this.timeout(30000);

        let url = baseURL + "/getAttrList";
        console.log("request "+ url);

        request({method:'POST', url : url, headers:{ "content-type": "application/json"},body:JSON.stringify({"uri":"http://www.jparksimulator.com/E-301.owl"})}, function(error, response, body) {
            if(error){
                console.log(error);
                done(error);
            }
            console.log(body)
            expect(response.statusCode).to.equal(200);
            var result = JSON.stringify([ { name: 'ValueOfHeatDutyOfE-301', value: '510.71283' },
                { name: 'ValueOf_EquipmentCost_E-301', value: '63800.0' },
                { name: 'ValueOf_x_E-301', value: '1.15414725598235E7' },
                { name: 'ValueOf_y_E-301', value: '140096.192340398' } ])
            expect(response.body).to.deep.equal(result);

            done();
        })

    })
});


describe('get /getChildrenSingle ', function () {
    it('returns status 200',function (done) {
        this.timeout(30000);

        let url = baseURL + "/getChildrenSingle";
        console.log("request "+ url);

        request({method:'POST', url : url, headers:{ "content-type": "application/json"},body:JSON.stringify({"uri":"http://www.jparksimulator.com/JurongIsland.owl"})}, function(error, response, body) {
            if(error){
                console.log(error);
                done(error);
            }
            console.log(body)
            expect(response.statusCode).to.equal(200);
            var result = [];
            expect(response.body).to.deep.equal(result);

            done();
        })

    })
});

describe('get /getSpecAttr ', function () {
    it('returns status 200',function (done) {
        this.timeout(3000);

        let url = baseURL + "/getSpecAttr";
        console.log("request "+ url);

        request({method:'POST', url : url, headers:{ "content-type": "application/json"},body:JSON.stringify({"uri":"http://www.jparksimulator.com/E-301.owl", "names":["V_molarF_3-1"]})}, function(error, response, body) {
            if(error){
                console.log(error);
                done(error);
            }
            console.log(body)
            expect(response.statusCode).to.equal(200);
            var result = [];
            expect(response.body).to.deep.equal(result);

            done();
        })

    })
});
