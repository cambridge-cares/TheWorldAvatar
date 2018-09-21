/**
 * Created by xiaochizhou on 2018/8/23.
 */

pragma solidity ^0.4.0;

contract DeliveryServiceRegistry {


    address[] public registeredServiceProviderAddressList;
    address private registryAddress;

    struct DeliveryServiceProvider{
        address providerAddress;
        int overallScore;
        int serviceCounter;
    }

    function DeliveryServiceRegistry(){

    }

    function getNumberOfRegisteredServices(){

    }

    function getAverageScoreOfRegisteredServices(){

    }

    function RegisterNewServiceProviderAddress(address newServiceProvider) public {
        registeredServiceProviderAddressList.push(newServiceProvider);
    }


    function DisqualifyAServiceProvider(address ServiceProvider) private{
        // When the score of a service provider falls under certain threshold,
        // remove it from the list

        // When scalping is detected, remove it from the list.
    }


    function validateServiceProvider() isRegistry {

    }


    function getRegisteredServiceProvider(uint threshold) public returns(address[]){

    }


    function getServiceAPIFromAddress(address serviceProvider) public returns(string)
    {

    }

    function scalpingSpotter(){

    }

    function evaluate() {

    }

    modifier isRegistry() {
        require(msg.sender ==registryAddress );
        _;
    }



}
