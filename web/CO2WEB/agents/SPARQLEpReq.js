/**
wrapper to request to an sparql endpoint
 */
const request = require('request'),
    SPARQLStr = require('./SPARQLStr'),
    async = require('async'),
    MAX_REUQESTS_ONETIME = 20
    
    
    
    var SPARQLEqReq =  {
    
    call : function(uris, updateQs, errCB){
        //first divide
        this.uris = uris
        this.updateQs = updateQs
        this.divide()
        async.each(this.Queue, this.singleCall.bind(this), errCB)

    },

    divide : function(){
     this.Queue = [];
      while(this.uris.length >= MAX_REUQESTS_ONETIME){
       let uris = this.uris.splice(0, MAX_REUQESTS_ONETIME)
       let updateQs = this.updateQs.splice(0, MAX_REUQESTS_ONETIME)
      this.Queue.push({uris, updateQs})
      }
      if(this.uris.length){
    let uris = this.uris.splice(0)
    let updateQs = this.updateQs.splice(0)
    this.Queue.push({uris, updateQs})
      }

     
    },
    singleCall: function ({uris, updateQs}, merrCB) {
        console.log("list of uris= "+uris)
        console.log("list of updatedQs= "+updateQs)

      let url = this.constructUrl(uris, updateQs)
        console.log("requesting SPARQL ENDPOINT")
        request
            .get(url)
            .on('error', function(err) {
                merrCB(err)
            })
    },
    
    
    
    constructUrl : function (uris, updateQs) {
       return 'http://www.theworldavatar.com/Service_Node_BiodieselPlant3/SPARQLEndPoint?uri=' + encodeURIComponent(JSON.stringify(uris)) + '&update=' + encodeURIComponent(JSON.stringify(updateQs)) + '&mode=update';
    }
    }
    
    
    module.exports = SPARQLEqReq