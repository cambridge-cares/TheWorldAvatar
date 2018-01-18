/**
 */
const request = require('request'),
    SPARQLStr = require('./SPARQLStr')
    
    
    
    var SPARQLEqReq =  {
    
    call: function (uris, updateQs, errCB) {
      let url = this.constructUrl(uris, updateQs)
        console.log("requesting SPARQL ENDPOINT")
        request
            .get(url)
            .on('error', function(err) {
                errCB(err)
            })
    },
    
    
    
    constructUrl : function (uris, updateQs) {
       return 'http://www.theworldavatar.com/Service_Node_BiodieselPlant3/SPARQLEndPoint?uri=' + encodeURIComponent(JSON.stringify(uris)) + '&update=' + encodeURIComponent(JSON.stringify(updateQs)) + '&mode=update';
    }
    }
    
    
    module.exports = SPARQLEqReq