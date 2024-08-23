/**
 * Module that calls sparql update API.
 */

/***
 * construct update query strings for all modification
 * @param uri
 * @param modifications
 */

function CallSPARQLUpdate(args) {
    this.unifyUri = args.unifyUri || null;//A unified uri that will overwrites all uris
    this.modifications = args.modifications || null;

    this.successCB = args.successCB || this.defaultSuccessCB;
    this.errorCB = args.errorCB || this.defaultErrorCB;

}


CallSPARQLUpdate.prototype = {
    //main function: calls to pack modis into update query str and ajax it out
    callUpdate : function () {
        if(this.modifications){
            let paras = this.constructUpdate(this.unifyUri, this.modifications);
            this.outputUpdate(...paras, this.successCB, this.errorCB);
            return;
        }
        console.log("warning: no modification list passed, update will not be sent")
    },

    constructUpdate : function (unifyuri, modifications) {
        let spoStrs = [], uris = [];
        modifications.forEach((item) => {
            let muri = unifyuri||item.uri;
            uris.push(muri, muri);
            spoStrs = spoStrs.concat(this.constructSingleUpdate(muri, item));
        });
        console.log(spoStrs);
        return [uris, spoStrs];
    },
    /**
     * construct a single update query
     * @param uri
     * @param attrObj
     * @returns {[*,*]}
     */
    constructSingleUpdate : function (uri, attrObj) {
        var spoDel = "DELETE WHERE {<" + uri + "#" + attrObj.name + "> <" + attrObj.p + "> " + "?o.}";
//TODO: add " for string

        var spoIns = "INSERT DATA {<" + uri + "#" + attrObj.name + "> <" + attrObj.p + "> " + "\""+attrObj.value + "\"^^<http://www.w3.org/2001/XMLSchema#"+attrObj.datatype+">.}";
        return [spoDel, spoIns];
    },
    //TODO: merge it with the one in PopUpmap, or think of how to seperately it from Popup map
    /***
     * Send output update queries
     * @param uris        array of uris to change, corresponding to updateQs
     * @param updateQs    array of SPARQL update strings
     * @param successCB   callback when success
     * @param errorCB      callback when err
     */
    outputUpdate: function(uris, updateQs, successCB, errorCB) {
    console.log(uris)
    console.log(updateQs)
    //TODO: construct uris && updateQs
    var myUrl = 'http://www.theworldavatar.com/Service_Node_BiodieselPlant3/SPARQLEndPoint?uri=' + encodeURIComponent(JSON.stringify(uris)) + '&update=' + encodeURIComponent(JSON.stringify(updateQs)) + '&mode=update';

    $.ajax({
        url: myUrl,
        method: "GET",
        contentType: "application/json; charset=utf-8",
        success: function (data) {//SUCESS updating
            successCB(data);

        },
        error: function (err) {
            errorCB(err);
        }
    });
},
    defaultSuccessCB : function (data) {
        console.log(data)

    },
    defaultErrorCB: function (err) {
        console.log(err);
    }


};
















