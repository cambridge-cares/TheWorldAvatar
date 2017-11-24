/**
 * This is a module that runs the heatwaste network python module.
 * Note:
 * TODO:The loc of the py script should be specified in config(and actually in owl file, in a much later future) instead of here
 * @type {runPython}
 */


const runPy = require('./runPython')
const config = require('../config')

function runHeatWasteNetwork(cb) {


//run the code, get results

    const heatWasteNetworkPy = config.heatWasteScript;
    const heatWasteNode = config.heatWasteNode;
    console.log(heatWasteNode)
    
    runPy(heatWasteNetworkPy, [heatWasteNode], (err, result)=>{

        if(err) {
            console.log(err)
            cb(err)
            return;
        }
        console.log( result)

        cb(null, result)
    });



}

module.exports = runHeatWasteNetwork





