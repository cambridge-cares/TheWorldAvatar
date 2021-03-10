
var expect =require("chai").expect;
var request = require("request");
var sinon  = require("sinon");
var nock = require("nock");
var PassThrough  = require("stream").PassThrough;

var fs = require('fs');

var watcherConfig = require("../util/changeWatcher");


describe("changeWatcher Module: callback return", function () {

it('callback concated result after request to each : test case 1 registeree, one file change', function (done) {
    let testFile = __dirname + "/test.txt";

    let fakeUrl = "http://dummy.com";
    let fakePath = "/change";
    console.log(testFile);
    var changeWatcher = watcherConfig({location:testFile, contentType:"text/plain"});

        //var fakeServer = sinon.fakeServer.create();
        //fakeServer.autoRespond = true;
        //var response = [ 200, { "Content-Type": "text/plain" }, "success" ];
        //fakeServer.respondWith( "POST", fakeUrl, response );

    nock(fakeUrl).post(fakePath).reply(200,"success");
    changeWatcher.register(fakeUrl+fakePath);


    changeWatcher.setWatch(function (err, results) {

        if(err){
            console.log(err);
            done(err);
        }

        console.log(results);
        expect(results).to.be.an('array');
        expect(results).to.have.length(1);

     //   expect(results[0]).to.deep.equal({informee: fakeUrl+fakePath, result:"success"});
        done();

    });

    //now modify file


      fs.appendFile(testFile, "another hello", function (err) {

          if(err){
              console.log("append File err");
              done(err);
          }
          console.log("append FIle success");
      });
//TODO: USE NOCK!!!!!!!!!!!!!!!!!!! for fake browser



}).timeout(10000);

it("callback concated result after request to each : test case 2 registeree, one file change", function(done){
    let testFile = __dirname + "/test2.txt";

    let fakeUrl1 = "http://dummy.com";
    let fakePath1 = "/change";
    console.log(testFile);
    var changeWatcher = watcherConfig({location:testFile, contentType:"text/plain"});


    let fakeUrl2 = "http://stupid.com";
    let fakePath2 = "/modify";

    nock(fakeUrl1).post(fakePath1).reply(200,"success");
    changeWatcher.register(fakeUrl1+fakePath1);

    nock(fakeUrl2).post(fakePath2).reply(200,"success");
    changeWatcher.register(fakeUrl2+fakePath2);

    changeWatcher.setWatch(function (err, results) {

        if(err){
            console.log(err);
            done(err);
        }

        console.log(results);
        expect(results).to.be.an('array');
        expect(results).to.have.length(2);

          expect(results[0]).to.deep.equal({informee: fakeUrl1+fakePath1, result:"success"});
        expect(results[1]).to.deep.equal({informee: fakeUrl2+fakePath2, result:"success"});

        done();

    });

    //now modify file


    fs.appendFile(testFile, "another hello", function (err) {

        if(err){
            console.log("append File err");
            done(err);
        }
        console.log("append FIle success");
    });
//TODO: USE NOCK!!!!!!!!!!!!!!!!!!! for fake browser
}).timeout(10000);



//TODO: TEST request body

//TODO: test dataOberserve route
});


describe("changeWatcher Module : request", function () {
beforeEach(function () {
   console.log("stub request")
    this.req = sinon.stub(request,"post");
});

afterEach(function(){
    //request.post.restore();
});

    it('should write the modified file as request body', function(done){
        let testFile = __dirname + "/test3.txt";
        var changeWatcher = watcherConfig({location:testFile, contentType:"text/plain"});
        let fakeUrl = "http://dummy.com";
        let fakePath = "/change";
        changeWatcher.register(fakeUrl+fakePath);

        var write;
        var mrequest = new PassThrough();
        write = sinon.spy(mrequest, 'write');
        this.req.callsArgWith(1, null,null, mrequest);//this stream is returned whenever request is called

        /**
        request.post({url:fakeUrl+fakePath}, function(err, r,body){
           if (err){
               throw err;
           }
            console.log(body);
        });
         ***/

        //modify file
        fs.appendFile(testFile, "another hello", function (err) {

            if(err){
                console.log("append File err");
                done(err);
            }
            console.log("append FIle success");

        });

        changeWatcher.setWatch(function (err, results) {
            if(err){
                done(err);
            }
            //now request is sent

            console.log(results);
            try {
                var content = fs.readFileSync(testFile);
                console.log(content);
                console.log(write.withArgs(content).called);
                expect(write.withArgs(content).called).to.be.true;
                done();

            } catch(err){
                done(err);
            }
        })


    }).timeout(50000);

});

describe("post path:dataObserve", function(){
    it('when accessed, register the url in posted data as an observer', function () {
        
    });
    it('when getInitialData is true, return the file in response', function () {
        
    })
    
});