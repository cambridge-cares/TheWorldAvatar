/**
 * Created by xiaochizhou on 2018/8/23.
 */

pragma solidity ^0.4.0;

contract DeliveryServicePerformanceRegistry {


    address[] public registeredServiceProviderAddressList;

    struct DeliveryServiceProvider{
        address providerAddress;
        int overallScore;
        int serviceCounter;
    }

    function DeliveryServicePerformanceRegistry(){

    }


    function RegisterNewServiceProviderAddress(address newServiceProvider) public {
        registeredServiceProviderAddressList.push(newServiceProvider);
    }


    function DisqualifyAServiceProvider(address ServiceProvider) private{
        // When the score of a service provider falls under certain threshold,
        // remove it from the list

        // When scalping is detected, remove it from the list.
    }


    function validateServiceProvider() {
        //
    }


    function getRegisteredServiceProvider(uint threshold) public returns(address[]){

    }


    function getServiceAPIFromAddress(address serviceProvider) public returns(string)
    {

    }

    function scalpingSpotter(){

    }

}
