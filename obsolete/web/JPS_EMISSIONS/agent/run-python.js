const config = require('../config/config');
const path = require('path');

// SYNCHRONOUS
// exports.runPython = function() {
//     const spawnSync = require('child_process').spawnSync;
//     var directory = path.join(config.projectDir, "python/SparqlQuery.py");
//     var process = spawnSync('python', [directory]);
//
//     return process.stdout.toString();
// };


// CALLBACK
// exports.runPython = function(cb) {
//
//     const spawn = require('child_process').spawn;
//
//     var directory = path.join(config.projectDir, "python/SparqlQuery.py");
//     var process = spawn('python', [directory]);
//
//     process.stdout.on('data', data => {
//         cb(data);
//     })
// };

exports.runPython = function(pythonScript, args){
    const spawn = require('child_process').spawn,
        pythonScriptFilePath = `./python/${pythonScript}`,
        params = args ? [pythonScriptFilePath, ...args] : [pythonScriptFilePath],
        process = spawn('python', params);

    let dataString = "";

    return new Promise((resolve, reject) => {
        // const process = spawn('python',
        //     [path.join(config.projectDir, `python/${pythonScript}.py`),
        //     JSON.stringify(config.projectDir)]);

        process.stdout.on('data', data => {
            dataString += data.toString();
            resolve(data);
        });

        process.stdout.on('end', () => {
            resolve(dataString);
        });
    });
};