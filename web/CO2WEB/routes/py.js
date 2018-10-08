var value = [1,2,3]


function ad(a, callback){
    setTimeout(function () {
        callback(null, [a*a, a*a]);
    },100)
    
}


var async = require('async')


async.concat(value, ad, function (err, results) {
    
    if(results){
        console.log(results)
    }
})