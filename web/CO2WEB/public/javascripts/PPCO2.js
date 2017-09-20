/**
 * Created by Shaocong on 8/31/2017.
 */
//TODO: listen to input:change, modify span accordingly


$(document).ready(function () {

    let curPath = window.location.href;
    let countrySelected;
    /*Country selector*****************************************/
    /*Percentage-input listener******************/
    $('#input-percentage').on('blur', function (e) {
        let percent = parseFloat($('#input-percentage').val());
        disInputPercent(percent);
        console.log(percent);

        if(!validateInputPercantage(percent)){
            displayMsg("Invalid percentage, please try again.","error")
            return;
        }
        //Send request to backend
        $.ajax({
            url: curPath+"/convertion",
            method:"POST",
            data:JSON.stringify({percent: percent, country:countrySelected}),
            contentType: "application/json; charset=utf-8",
            success: function (data) {
                //Update display
                console.log(data);
                displayConvertResult(data);
            },
            error : function () {
                displayMsg("Can not connect to server" , "error")
            }
        });

        
        function validateInputPercantage(input) {
            console.log("Check input:" + input)
            if (isNaN(input)){
                return false;
            }
            if(input < 1 || input > 100) {
                return false;
            }
            return true;
        }


    });

    var tableP =         $("#table-panel");


    function displayListByCountry(list) {
        tableP.empty();
        let table = "<table><tr><td>Name of Power Plant</td><td>Capacity</td><td>Type</td><td>Co2 Emission(tonnes/h)</td></tr>";
        let total  = 0;

        list.forEach(function (item) {
            let name = getNameOfUrl(item.name);
             let type = getsimpleType(item.type)
            table+="<tr><td>"+name+"</td><td>"+item.capacity+"</td><td>"+type+"</td><td>"+parseFloat(item.emission).toFixed(2)+"</td></tr>";
           total+=parseFloat(item.emission);
        });

        table+="</table>";
        tableP.append(table);

        console.log("total: "+total);
        displayCountryTotal(total.toFixed(2));
    }



    function getNameOfUrl(url){
        return url.split('#')[1];
    }
    function getsimpleType(typeuri){
        return typeuri.split('#')[1];
    }
    
    $("select#country-select").on('change', function () {
        countrySelected = $("select#country-select option:checked").val();
        countrySelected = JSON.parse(countrySelected);
        countrySelected = countrySelected["country"];
        console.log(countrySelected)
        disInputCountry(countrySelected);

        $.ajax({
            url: curPath+"/listbycountry",
            method:"POST",
            data:JSON.stringify({country: countrySelected}),
            contentType: "application/json; charset=utf-8",
            success: function (list) {
                //Update display
                displayListByCountry(list);
            },
            error : function () {
                displayMsg("Can not connect to server" , "error")
            }
        });

    });


    
    
    /*Err Msg Bar************************************************************************/
    var template = function (msg, type) {

        return "<p class='alert alert-" + type + "'>" + msg + "</p>";
    };

    function displayMsg(msg, type) {
        //TODO: swithc type
        $("#err-msg-panel").html("");
        $("#err-msg-panel").append(template(msg, type));

    }

    /*Display DOM**********************************************************************/
    function disInputPercent(percent){
        $("#input-percentage-echo").text(percent);
    }
    function disInputCountry(country){
        $("#select-country-echo").text(country);
    }
    function  displayConvertResult(result) {
        $("#convert-result-echo").text(result);

    }
    function  displayCountryTotal(result) {
        $("#country-total-echo").text(result);

    }
});