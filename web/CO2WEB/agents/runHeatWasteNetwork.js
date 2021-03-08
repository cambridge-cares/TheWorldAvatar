/**
 * This is a module that runs the heatwaste network python module.
 * @type {runPython}
 */


const runPy = require('./RunPython')
const config = require('../config')

function runHeatWasteNetwork(plantData, cb) {


//run the code, get results



    const heatWasteNetworkPy = config.heatWasteScript;
    const heatWasteNode = config.heatWasteNode;
    console.log(heatWasteNode)
    //get groups of data from heatwaste network, and send it


    runPy(heatWasteNetworkPy, plantData, (err, result)=>{

        if(err) {
            console.log(err)
            cb(err)
            return;
        }
        console.log( result)

        cb(null, result)


    })







};
module.exports = runHeatWasteNetwork
