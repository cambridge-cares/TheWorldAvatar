/**
 * Created by xiaochizhou on 2018/8/23.
 */
const path = require('path');
const fs = require('fs');
const solc = require('solc');

const DeliveryServiceEvaluationPath = path.resolve(__dirname,'contracts','Test.sol');
const source = fs.readFileSync(DeliveryServiceEvaluationPath, 'utf-8');


module.exports = solc.compile(source, 1).contracts[':Test'];


