/**
 * Created by xiaochizhou on 2018/9/2.
 */

const HDWalletProvider = require('truffle-hdwallet-provider');
const Web3 = require('web3');
const { interface , bytecode } = require('./compile');

const provider = new HDWalletProvider(
    'device autumn cereal assault guess cat awkward bring flock square field swing'
    , 'https://rinkeby.infura.io/v3/8ee553dc456b4ee3ad6fe42de3930b0c'
);

const web3 = new Web3(provider);



const deploy = async() => {
    const accounts = await web3.eth.getAccounts();
    console.log('Deploying from an account', accounts[0]);

    const result = await new web3.eth.Contract(JSON.parse(interface))
        .deploy({data: bytecode, arguments: []})
        .send({ gas: '500000', from: accounts[0]});

    console.log('Contract deployed to ', result.options.address);

};

deploy();

