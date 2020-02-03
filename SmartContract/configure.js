// This file holds all the configuration needed
let fs = require('fs');
let json = JSON.parse(fs.readFileSync('./config.json', 'utf8'));
let contract_address = json['address'];
let abi = json['abi'];

module.exports = {
    contract_address: contract_address,
    abi: abi
};