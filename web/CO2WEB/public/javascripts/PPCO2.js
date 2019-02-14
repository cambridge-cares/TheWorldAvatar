
$(document).ready(function () {





    const toggleDisplay = elemId => {
        let x = document.getElementById(elemId);
        if (x.style.display !== 'block') {
            x.style.display = 'block';
        } else {
            x.style.display = 'none';
        }
    };

    $("#readme-button").click(function() {
        toggleDisplay("readme-text");
    });

    document.addEventListener("click", function(evt) {
        var readmeButtonElement = document.getElementById('readme-button'),
            readmeTextElement = document.getElementById('readme-text'),
            targetElement = evt.target;  // clicked element

        if (targetElement == readmeButtonElement || targetElement == readmeTextElement) {
            return; //readme-button or readme-text is clicked. do nothing.
        }

        if(readmeTextElement.style.display === 'block') {
            readmeTextElement.style.display = 'none';
        }
    });





    let curPath = window.location.href;
    let countrySelected;
    let lastPercentInput;

    let tableP = $("#table-panel");//table that displays pp list in a country
    let panel = $("#err-msg-panel");

    /*Percentage-input listener******************/
    $('#input-percentage').on('blur', function (e) {//user moves out of input field
        enterPercentage();//=>percentage is entered, handle it!
    });

    $(document).keypress(function(e) {
        if(e.which == 13) {//if press enter!
            enterPercentage();//=>percentage is entered, handle it!

        }
    });


    /**
     * Handler when new percentage is entered
     */
    function enterPercentage() {
        let percent = $('#input-percentage').val();
        cleanMsg();
        if(percent === "" ){//percent === lastPercentInput
            console.log("have not enter any percentage or same as last, just return")
            return;//return without doing anything
        }
         lastPercentInput = percent;
         percent = parseFloat(percent);
        if(!countrySelected){
            displayMsg("Have not chosen a country.","danger")
            return;
        }
        if(!validateInputPercantage(percent)){
            displayMsg("Invalid percentage, please try again.","danger")
            return;
        }
        disInputPercentEcho(percent);
        console.log(percent);
        //Send request to backend
        $.ajax({
            url: curPath+"/convertion",
            method:"POST",
            data:JSON.stringify({percent: percent, country:countrySelected}),
            contentType: "application/json; charset=utf-8",
            success: function (data) {
                //Update display
                console.log(data);
                displayConvertResult(data.toFixed());
            },
            error : function () {
                displayMsg("Can not connect to server" , "danger")
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
    }


    /***
     * Write table dom to display a pp list by country
     * @param list
     */
    function displayListByCountry(list) {
        tableP.empty();
        //let table = "<table><tr><td>Name of Power Plant</td><td>Capacity</td><td>Type</td><td>CO<sub>2</sub> Emission(tonnes/h)</td></tr>";
		let table = "<table><tr><td>Capacity</td><td>Type</td><td>CO<sub>2</sub> Emission(tonnes/h)</td></tr>";
        let total  = 0;

        list.forEach(function (item) {
            let name = getNameOfUrl(item.name);
             let type = getsimpleType(item.type)
            //table+="<tr><td>"+name+"</td><td>"+item.capacity+"</td><td>"+type+"</td><td>"+parseFloat(item.emission).toFixed(2)+"</td></tr>";
			table+="<tr><td>"+item.capacity+"</td><td>"+type+"</td><td>"+parseFloat(item.emission).toFixed(2)+"</td></tr>";
           total+=parseFloat(item.emission);
        });

        table+="</table>";
        tableP.append(table);

        console.log("total: "+total);
        displayCountryTotal(total.toFixed());
    }



    function getNameOfUrl(url){
        return url.split('#')[1];
    }
    function getsimpleType(typeuri){
        return typeuri.split('#')[1];
    }

    /*Country selector*****************************************/
    $("select#country-select").on('change', function () {//when a new country is selected
        countrySelected = $("select#country-select option:checked").val();
        countrySelected = JSON.parse(countrySelected);
        countrySelected = countrySelected["country"];
        console.log(countrySelected)
        disInputCountry(countrySelected);
        cleanMsg();
        $.ajax({
            url: curPath+"/listbycountry",
            method:"POST",
            data:JSON.stringify({country: countrySelected}),
            contentType: "application/json; charset=utf-8",
            success: function (list) {
                console.log(list)
                //Update display
                displayListByCountry(list);
                //clean convert area
                disInputPercentEcho("");
                displayConvertResult("");
                //TODO: clean input field
                displayInputPercent("");
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
        cleanMsg();
        panel.append(template(msg, type));

    }
    function cleanMsg() {
        panel.html("");

    }

    /*Display DOM**********************************************************************/
    function disInputPercentEcho(percent){
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
    function displayInputPercent(input) {
        $('#input-percentage').val(input);
    }
});