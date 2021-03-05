/**
 Test fileConnection module, which extracts all connections out of current grOWLs, starting from top node on disk
 */

/*******IMPORT**********/
var expect = require("chai").expect;
var util = require("util");
var request = require("request");
var path = require("path");
var connections = require("../agents/fileConnection2Way.js");


var config = require("../config");

var topNodeAddress = config.worldNode;

//expect(conn).to.have.property('source');

describe("extract connections", function () {
    it('works on ttl files', function (done) {
        let localTtl = 'D:\\workwork\\jpsweb\\irp3-WebJPS-git\\CO2WEB\\testFiles\\test.ttl';

        let opts = {useSharp:true, topnode:localTtl,supQuery:`
PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/> 
    SELECT ?parent  ?uri
    WHERE{
     ?parent  system:hasIRI ?uri.
     }
`};
        this.timeout(30000);
        connections.processSingle(opts).then((conn)=> {
                console.log(conn);
                let sampleConn = {source:'http://example.org/#green-goblin',target:'test',label:'',level:1}
                expect(conn).to.be.an('array');
                    expect(conn).to.have.lengthOf(1);
            expect(conn[0]).to.deep.equal(sampleConn);

            done()
            }
        )

    });




});
