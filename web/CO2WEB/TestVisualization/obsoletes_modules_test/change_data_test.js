/**
 * Test the registerer module: register to data-changing node
 * Test the /change route for data-changing node to post changed data
 * When /change route successfully connected, an update event should be emitted to socket clients
 * Test if socket client can receive this event and along with it, the changed data
 *
 */
//test register
var expect = require("chai").expect;
var util = require("util");
var nock = require("nock");

var registerer = require("../../agents/register2DataChangeObsolete");
var config = require("../../config")


var io = require('socket.io-client')
var request = require('request')

var app = require('../../app')
var fakeReply = {
    fakeData: 1
};
//test deregister

describe('registerer', function () {

    beforeEach(function () {
        //use nock to fake it
        console.log(config.bmsUrlPath)
        console.log(config.registerPath);
        console.log(config.registerUrl)
        nock(config.registerUrl)
            .post("/"+config.registerPath,{
                url:config.myUrlPath, getInitData:true, register:true
            })
            .reply(200, {
                fakeData: 1
            });
    });
    it('register by post to desitination url', function (done) {




        registerer.register(config.bmsUrlPath, config.myUrlPath, function (err, data) {
            if(err){
                done(err);
            }

            expect(JSON.parse(data)).to.deep.equal({fakeData:1});
            done();
        })


    })

});


//test post change to /change route
describe('post to /change', function () {

    beforeEach(function (done) {
        socket = io.connect(config.changeUrl, {
            'reconnection delay' : 0
            , 'reopen delay' : 0
            , 'force new connection' : true
        });

        socket.on('connect', function(data) {

            done();
        });
        socket.on('disconnect', function() {
            console.log('disconnected...');
        })

        nock(config.registerUrl)
            .post("/"+config.registerPath)
            .reply(200, fakeReply);

        app.listen(3000, function () {
            console.log('Server listening on port 3000');
        });


    });

    afterEach(function(done) {
        // Cleanup
        if(socket.connected) {
            console.log('disconnecting...');
            socket.disconnect();
        } else {
            // There will not be a connection unless you have done() in beforeEach, socket.on('connect'...)
            console.log('no connection to break...');
        }
        done();
    });

    it("get success", function (done) {
      //now request
        request.post({url:config.changeUrl+"/"+config.changePath, body: JSON.stringify({data:"world"}) }, function (err, res, body) {
            if(err){
                done(err);
            }
            console.log("request to change url : " + util.inspect(body));
            expect(body).to.equal('success');
            done();
        });

    }).timeout(5000);

    it("sends the data via socket",function (done) {
        //request first to trigger /change
        request.post({url:config.changeUrl+"/"+config.changePath, body: JSON.stringify({data:"world"}),headers: {
            'content-type': "application/json"
        } }, function (err, res, body) {
            if(err){
                done(err);
            }
            console.log("request to change url : " + util.inspect(body));
        });

        // update event should be emitted on socket and data sent to /change will be sent along socket
        socket.on('update', function(data) {
            console.log(data);
            console.log('test client connected');
            expect(JSON.parse(data)).to.deep.equal({data:"world"});
            done();
        });

    }).timeout(5000);
});



//TODO: server close deregister