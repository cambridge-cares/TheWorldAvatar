/**
 * Created by Shaocong on 10/6/2017.
 */
const getB3C = require('./GetB3Coordi')
const fileC = require('./fileConnection')
const request = require('request')
const async = require('async')
const convertC = fileC.convertCoordinate;

getB3C(function (err, result) {
    if(err){
        throw err;
    }

    console.log(result);
    let dx = 18.070053359493613, dy = 14.979029614682076;

    function getSample(uri, lng, lat) {
        let name = uri.split('/');
        if (name.length < 2){
            throw new Error("can not get simple name");
        }
        let sname= name[name.length - 1];
       let snamearr = sname.split('.');
        sname = snamearr[0];

        let convertedC = convertC(lng, lat, true);





        let newX = convertedC.x + 2*dx;
        let newY = convertedC.y + 2*dy;
        let deleteStrx = "DELETE WHERE {<"+uri+"#ValueOf_x_"+sname+"> <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue> ?o.}";


        let insertStrx = "INSERT DATA {<"+uri+"#ValueOf_x_"+sname+"> <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue> \""+ newX +"\"^^<http://www.w3.org/2001/XMLSchema#decimal>.}";

      let deleteStry = "DELETE WHERE {<"+uri+"#ValueOf_y_"+sname+"> <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue> ?o.}";



        let insertStry = "INSERT DATA {<"+uri+"#ValueOf_y_"+sname+"> <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue> \""+ newY +"\"^^<http://www.w3.org/2001/XMLSchema#decimal>.}";

        return [deleteStrx, insertStrx,deleteStry, insertStry];

      //  return [deleteStrx,deleteStry];

    }
//http://www.jparksimulator.com/P-301.owl#ValueOf_x_P-301

//http://www.jparksimulator.com/P-301.owl#ValueOf_x_P-301

let sample  = `DELETE WHERE {<http://www.jparksimulator.com/P-301.owl#ValueOf_x_P-301> <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue> ?o.}"
1
:
"INSERT DATA {<http://www.jparksimulator.com/P-301.owl#ValueOf_x_P-301> <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue> "1.15414820950720E7"^^<http://www.w3.org/2001/XMLSchema#decimal>.}"

`

    let queryStrs = [];
 result.forEach(function (entity) {

     let str4 = getSample(entity.uri, entity.location.lng, entity.location.lat);

     let uri = entity.uri
     queryStrs.push({uri: [ uri, uri,uri, uri], updateQ: str4});


    })


    console.log(queryStrs);
console.log


    function requestChild(paras, callback) {//http request  to get child file

        let uris= paras.uri;
        let updateQs = paras.updateQ;
        console.log(uris)
        console.log(updateQs)
        console.log("request for "+uris[0])

        var myUrl = 'http://www.theworldavatar.com/Service_Node_BiodieselPlant3/SPARQLEndPoint?uri=' + encodeURIComponent(JSON.stringify(uris)) + '&update=' + encodeURIComponent(JSON.stringify(updateQs)) + '&mode=update';

        request.get(myUrl, {timeout: 10000, agent:false},function (err, response, body) {

            if (err || response.statusCode !== 200) { //request failed
                console.log(err);
                callback(null, null); // return null
                return;

            }

            if (response.statusCode === 200) {//request success
                //logger.debug("req: " + iri);
                console.log("success")
                return;
            }

            logger.debug("why the fuck did anyone get to this point");
            callback(null, null);
        });

    }


    async.each(queryStrs, requestChild, function (err) {
        if(err){
            console.log(err);
        }

    })



})