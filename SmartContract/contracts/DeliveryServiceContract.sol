/**
 * Created by xiaochizhou on 2018/8/23.
 */

pragma solidity ^0.4.24;

contract DeliveryServiceContract {

    event PackageLocationChanged(string location, address sender);
    event DeadlineMissed();

    // It should also be monitoring physical activities such as

    uint public estimatedDeliveryTime;
    uint public deadlineOffSet;

    string public state;

    function DeliveryServiceContract(uint newEstimatedDeliveryTime, uint newDeadlineOffSet) public {
        // To initialize an instance of the contract, you need ...
        estimatedDeliveryTime = newEstimatedDeliveryTime;
        deadlineOffSet = newDeadlineOffSet;

    }

    function makeFinalPayment() public {

    }

    function setDeliveryState(string newState) public {
        state = newState;
    }

    function getState() public view returns(string){
        return state;
    }

    function deposit() public payable {

    }



    // When the package is delivered, call the contract
    // and update the evaluation to the registry.

}