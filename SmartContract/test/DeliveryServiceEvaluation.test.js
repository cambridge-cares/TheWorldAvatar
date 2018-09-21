/**
 * Created by xiaochizhou on 2018/8/24.
 */
 
const assert = require('assert');
const ganache = require('ganache-cli');
const Web3 = require('web3');
const web3 = new Web3(ganache.provider());
const { interface , bytecode } = require('../compile');

let accounts;
let evaluation;
// deploy a new contract
// manipulate the contract
// make an assertion about the contract

// beforeEach
// it
// it


beforeEach(async () => {
   accounts = await web3.eth.getAccounts();
   // deploy the contract
    evaluation = await new web3.eth.Contract(JSON.parse(interface))
       .deploy({data:bytecode,arguments:[0,0,3,4]})
       .send({from: accounts[0], gas: '1000000'})

});

describe('Evaluation', () => {
   it('deploys a contract', () => {
       assert.ok(evaluation.options.address);
   });

   it('Get distance', async () => {
      const distance = await evaluation.methods.getPrice().call();
      console.log('distance',distance);
      assert.equal(distance,100);
   });

   it('The updated state to be', async () => {
      const astate = await evaluation.methods.setDeliveryState('delivered').send({from: accounts[0]});
      const state = await evaluation.methods.getState().call();
      console.log(state);
      console.log(astate);

   });



});