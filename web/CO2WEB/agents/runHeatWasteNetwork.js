const runPy = require('./runPython')

function runHeatWasteNetwork(cb) {
    //TODO: The coordinates ought to be get from owl files but we don;t have the plant files at the moment


//run the code, get results

    const heatWasteNetworkPy = "C:/Users/Shaocong/WORK/webJPSGit/irp3-WebJPS-git/CO2WEB/agents/WHR_network_optimization_trim.py";

    runPy(heatWasteNetworkPy, null, (err, result)=>{

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










