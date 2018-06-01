$(function(){
     
    
    $('#start').click(function(){
//        console.log("start simulation")
        //$('#start').attr("disabled", true);
    	
        var carbontax = parseFloat($('#carbontax').val());
        var interestfactor = parseFloat($('#interestfactor').val());
        var intmarketpricefactor = parseFloat($('#intmarketpricefactor').val());
        var intmarketlowestprice = JSON.parse($('#intmarketlowestprice').val());
      var list =[carbontax,interestfactor,intmarketpricefactor,intmarketlowestprice];
      var vartime =["timeone","timeten","timefifty","timehundred"];
       // alert("you click the run button and marketlowestprice= " +intmarketlowestprice);
//        console.log(xmin +" "+xmax + " " + ymin + " " + ymax)
        
        
     /* $.ajax('http://www.theworldavatar.com/JPS_MEN/MENTableAgent?calculationparameter='+encodeURIComponent(JSON.stringify({'carbontax':carbontax,'interestfactor':interestfactor, 'intmarketpricefactor':intmarketpricefactor, 'intmarketlowestprice':intmarketlowestprice}).replaceAll('"',"'")))
		.done(function () {

			$.get("MenTableAgent", function(response) {
			    console.log(response);
			});
        })
        
        .fail(function () {
            console.log("error")
        })*/
      
        $.getJSON('/JPS_MEN/MENTableAgent',
                {
        			listOfInputs: JSON.stringify(list)
                },
                function(data) {
                    
               var result = data;
               console.log("data of result= "+JSON.stringify(result)); 
               
              
               console.log("data of transportationcost11= "+result.timeten[0].totalTransportationCost); 
       	   // alert("you click the run button and finish "+result[1][1]);	
               
               document.getElementById("cell11").innerHTML = result.timeone[0].totalTransportationCost;
               document.getElementById("cell12").innerHTML = result.timeone[0].totalMaterialPurchaseCost;
               document.getElementById("cell13").innerHTML = result.timeone[0].totalInstallationCost;
               document.getElementById("cell14").innerHTML = result.timeone[0].totalCO2Emission;
               document.getElementById("cell15").innerHTML = result.timeone[0].totalCO2EmissionCost;
               document.getElementById("cell16").innerHTML = result.timeone[0].totalTransportCost1year;
               document.getElementById("cell17").innerHTML = result.timeone[0].totalTransportCost10year;
               document.getElementById("cell18").innerHTML = result.timeone[0].totalTransportCost50year;
               document.getElementById("cell19").innerHTML = result.timeone[0].totalTransportCost100year;
               
               document.getElementById("cell21").innerHTML = result.timeten[0].totalTransportationCost;
               document.getElementById("cell22").innerHTML = result.timeten[0].totalMaterialPurchaseCost;
               document.getElementById("cell23").innerHTML = result.timeten[0].totalInstallationCost;
               document.getElementById("cell24").innerHTML = result.timeten[0].totalCO2Emission;
               document.getElementById("cell25").innerHTML = result.timeten[0].totalCO2EmissionCost;
               document.getElementById("cell26").innerHTML = result.timeten[0].totalTransportCost1year;
               document.getElementById("cell27").innerHTML = result.timeten[0].totalTransportCost10year;
               document.getElementById("cell28").innerHTML = result.timeten[0].totalTransportCost50year;
               document.getElementById("cell29").innerHTML = result.timeten[0].totalTransportCost100year;
               
               document.getElementById("cell31").innerHTML = result.timefifty[0].totalTransportationCost;
               document.getElementById("cell32").innerHTML = result.timefifty[0].totalMaterialPurchaseCost;
               document.getElementById("cell33").innerHTML = result.timefifty[0].totalInstallationCost;
               document.getElementById("cell34").innerHTML = result.timefifty[0].totalCO2Emission;
               document.getElementById("cell35").innerHTML = result.timefifty[0].totalCO2EmissionCost;
               document.getElementById("cell36").innerHTML = result.timefifty[0].totalTransportCost1year;
               document.getElementById("cell37").innerHTML = result.timefifty[0].totalTransportCost10year;
               document.getElementById("cell38").innerHTML = result.timefifty[0].totalTransportCost50year;
               document.getElementById("cell39").innerHTML = result.timefifty[0].totalTransportCost100year;
               
               document.getElementById("cell41").innerHTML = result.timehundred[0].totalTransportationCost;
               document.getElementById("cell42").innerHTML = result.timehundred[0].totalMaterialPurchaseCost;
               document.getElementById("cell43").innerHTML = result.timehundred[0].totalInstallationCost;
               document.getElementById("cell44").innerHTML = result.timehundred[0].totalCO2Emission;
               document.getElementById("cell45").innerHTML = result.timehundred[0].totalCO2EmissionCost;
               document.getElementById("cell46").innerHTML = result.timehundred[0].totalTransportCost1year;
               document.getElementById("cell47").innerHTML = result.timehundred[0].totalTransportCost10year;
               document.getElementById("cell48").innerHTML = result.timehundred[0].totalTransportCost50year;
               document.getElementById("cell49").innerHTML = result.timehundred[0].totalTransportCost100year;
            	
               
              /* document.getElementById("concentration10").innerHTML = concentrations[3];
               document.getElementById("concentration20").innerHTML = concentrations[4];
               document.getElementById("concentration30").innerHTML = concentrations[5];*/
                    
                   /* for (var i = 0; i < arrayLength; i++) {

                        try {
                           osmb.addGeoJSON(geojson[i]);
                        	
                        }
                        catch(err) {
                            console.log(err.name)
                        }

                    }*/
                })
                
                
    })
});

    
//String.prototype.replaceAll = function(search, replacement) {
  //  var target = this;
    //return target.replace(new RegExp(search, 'g'), replacement);
//};