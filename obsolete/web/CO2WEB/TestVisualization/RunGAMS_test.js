/**
 */
var expect = require("chai").expect;

var runGams = require('../agents/RunGAMSPredefinedOwlFile')






describe('runGAMS', function (done) {
    it('delete old files', function (done) {
        this.timeout(50000)
        runGams(0, function (err, result) {
            console.log(result)
            done()
            
        })
        
        
    })
})