const express = require('express');
const router = express.Router();

const spawn = require('child_process').spawn;

const config = require('../config/config');

const runPython = require('../agent/run-python');

router.post('/TestEndPoint', (req, res, next) => {
    req.setTimeout(3600000);
    const carbonPrice = req.body.carbonPrice;
    const technologyLearningRate = req.body.technologyLearningRate;

    // const promise = runPython.runPython('PowerplantEmissionCalculator.py',
    //     [JSON.stringify(config.projectDir), 'subcritical', 'bituminous']);
    const promise = runPython.runPython('latest/main.py',
        [JSON.stringify(config.projectDir), carbonPrice, technologyLearningRate]);

    promise.then(result => { res.send(result) });

    // res.json({'message': 'SUCCESSFUL'});
});

// STATIC
// router.get('/HeatMap', function(req, res, next) {
//
//     console.log(JSON.stringify(config.projectDir));
//     const promise = runPython.runPython('EmissionPlotter.py',
//         [JSON.stringify(config.projectDir)]);
//
//     promise.then(result => { res.send(result) });
//
// });

// router.get('/CommittedEmission', function(req, res, next) {
//
//     const process = spawn('python',
//         ["./python/CommittedEmissionPlotter.py", JSON.stringify(config.projectDir), "20"]);
//
//     process.stdout.on('data', data => {
//         res.send(data);
//     })
// });
//
// router.get('/GenerationCost', function(req, res, next) {
//
//     const process = spawn('python',
//         ["./python/GenerationCostPlotter.py", JSON.stringify(config.projectDir)]);
//
//     process.stdout.on('data', data => {
//         res.send(data);
//     })
// });
//
// router.get('/AvoidanceCost', function(req, res, next) {
//
//     const process = spawn('python',
//         ["./python/AvoidanceCostPlotter.py", JSON.stringify(config.projectDir)]);
//
//     process.stdout.on('data', data => {
//         res.send(data);
//     })
// });

module.exports = router;