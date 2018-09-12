/**
 * Created by xiaochizhou on 2018/8/23.
 */

pragma solidity ^0.4.24;

contract DeliveryServiceEvaluation {


    uint public estimatedDeliveryTime;
    uint public deadlineOffSet;

    string public state;

    function DeliveryServiceEvaluation(uint newEstimatedDeliveryTime, uint newDeadlineOffSet) public {
        // To initialize an instance of the contract, you need ...
    estimatedDeliveryTime = newEstimatedDeliveryTime;
    deadlineOffSet = newDeadlineOffSet;

    }

    function setDeliveryState(string newState) public {
        state = newState;
    }

    function getState() public view returns(string){
        return state;
    }




    // When the package is delivered, call the contract
    // and update the evaluation to the registry.

}