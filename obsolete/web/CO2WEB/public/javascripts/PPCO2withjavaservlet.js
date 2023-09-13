
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
	
	sumofallco2();

  //let domainpath="http://localhost:8080";
	let domainpath="http://www.theworldavatar.com";
    let countrySelected;
    let lastPercentInput;
	let totalcapacity=0;

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

		//calculation of the percentage
		console.log("totalcapacity= "+totalcapacity);
		var conversionresult= percent/100*totalcapacity*(0.8-0.375);
		console.log("result of conversion= "+conversionresult);
		displayConvertResult(conversionresult.toFixed());
		

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
        let table = "<table><tr><td>Capacity(MW)</td><td>Type</td><td>CO<sub>2</sub> Emission(tonnes/h)</td></tr>";
		//let table = "<table><tr><td>Name of Power Plant</td><td>Capacity(MW)</td><td>Type</td><td>CO<sub>2</sub> Emission(tonnes/h)</td></tr>";
		//let table = "<table><tr><td>Capacity</td><td>Type</td><td>CO<sub>2</sub> Emission(tonnes/h)</td></tr>";
        let total  = 0;

        list.forEach(function (item) {
            let name = getNameOfUrl(item.entity);
             let type = getsimpleType(item.generation)
			  table+="<tr><td>"+item.vcapa+"</td><td>"+type+"</td><td>"+parseFloat(item.vemission).toFixed(2)+"</td></tr>";
            //table+="<tr><td>"+name+"</td><td>"+item.vcapa+"</td><td>"+type+"</td><td>"+parseFloat(item.vemission).toFixed(2)+"</td></tr>";
			//table+="<tr><td>"+item.capacity+"</td><td>"+type+"</td><td>"+parseFloat(item.emission).toFixed(2)+"</td></tr>";
           total+=parseFloat(item.vemission);
        });

        table+="</table>";
        tableP.append(table);

        console.log("totalemission: "+total);
        displayCountryTotal(total.toFixed(2));
    }
	
	function collectallthecoalplant(list) {
		totalcapacity=0;
		list.forEach(function (item) {
			let type=getsimpleType(item.generation);
			
			if (type==="CoalGeneration")
			{
				totalcapacity+=parseFloat(item.vcapa);
			}
			
			});
			return totalcapacity;
	}
	
		function sumofallco2() {
		
		var url = "http://www.theworldavatar.com:80/damecoolquestion/worldpowerplantsinmemory/query";
		console.log("it's going here");
		var value2=0;
		var qsemissionsum= `
			PREFIX cp:<http://dbpedia.org/resource/>         
         PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
		 PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> 
		 PREFIX j4:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#> 
		 PREFIX j5:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> 
		 PREFIX j8: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
		 PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

        select (sum(xsd:double(?vemission)) as ?vemissionsum)
        {graph ?g
			{?entity  j3:realizes ?generation.		
		?generation j5:hasEmission ?emission.
		?emission j2:hasValue ?valueemission.
		?valueemission j2:numericalValue ?vemission.
          }
		     }`;
			 
		var http = new XMLHttpRequest();
		
		http.open("GET", url+"?query="+encodeURIComponent(qsemissionsum), true);
			http.send();
		http.onreadystatechange = function()
{
    if(http.readyState == 4 && http.status == 200) {
        console.log(http.responseText);
		var myObj = JSON.parse(this.responseText);
		value1=myObj.results;
		//console.log("value= "+JSON.stringify(value1));
		//var myObj2 = JSON.parse(value1);
		value2=value1.bindings[0].vemissionsum.value;

		//console.log("value2= "+JSON.stringify(value2));
		var totalemission=JSON.stringify(value2);
		var modif=totalemission.replace('"','');
		
		distotalemission(parseFloat(modif).toFixed(2));
    }

}
	
	//distotalemission(JSON.stringify(value2));

	}


    function getNameOfUrl(url){
        return url.split('#')[1];
    }
    function getsimpleType(typeuri){
        return typeuri.split('#')[1];
    }

    //Country selector*****************************************

	    $("select#country-select").on('change', function () {//when a new country is selected
        countrySelected = $("select#country-select option:checked").val();
        countrySelected = JSON.parse(countrySelected);
        countrySelected = countrySelected["country"];
        
        disInputCountry(countrySelected);
        cleanMsg();
		
		var country=countrySelected.split("resource/")[1];
		//qsplantentity=qsplantentity.replace(country, countrySelected.split("resource/")[1]);
		
			var qsplantentity = `
			PREFIX cp:<http://dbpedia.org/resource/>         
         PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
		 PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> 
		 PREFIX j4:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#> 
		 PREFIX j5:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> 
		 PREFIX j8: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>

        select distinct ?entity ?vcapa ?generation ?technology ?vemission ?fuel ?vyear
        {graph ?g
			{?entity j2:hasAddress cp:${country}.

        ?entity  j3:realizes ?generation.
		?generation j8:usesGenerationTechnology ?technology.
		?generation j8:consumesPrimaryFuel ?fuel.
		
		?generation j5:hasEmission ?emission.
		?emission j2:hasValue ?valueemission.
		?valueemission j2:numericalValue ?vemission.
		FILTER regex(STRBEFORE(STR(?valueemission),"#"), STRBEFORE(STR(?entity),"#")) .
		
		
		?entity  j4:designCapacity ?capa.
		?capa j2:hasValue ?valuecapa.
		?valuecapa j2:numericalValue ?vcapa.
		
		?entity j8:hasYearOfBuilt ?year.  
				?year   j2:hasValue ?valueyear. 
				?valueyear   j2:numericalValue ?vyear.
    
          }
		     }`;

        $.ajax({
            url: domainpath+"/JPS_CO2EMISSIONS/QueryPowerPlants",
            method:"GET",
            data: {query: qsplantentity},
            contentType: "json; charset=utf-8",
            success: function (list) {
                				
				for (var i = 0; i < list.results.length; i++) {
				var counter = list.results[i];
				
			}		
                //Update display
				//sumofallco2();
                displayListByCountry(list.results);
				collectallthecoalplant(list.results);
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
	function distotalemission(result){
        $("#co2Value").text(result);
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