$(function(){
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
    
    $('#start').click(function(){
    	
			$("#buttonrun").append('<img id="myProgressBar" style="width:100px;height:100px;" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/>')

		
		
		
        var carbontax = parseFloat($('#carbontax').val());
        var interestfactor = "1.0";
        var intmarketpricefactor = parseFloat($('#intmarketpricefactor').val());
        var intmarketlowestprice = JSON.parse($('#intmarketlowestprice').val());
      
       var timefactor = $('#timefactor').val().split(",");
      var list =[carbontax,interestfactor,intmarketpricefactor,intmarketlowestprice,timefactor];
      
      
      var query = {
    		"CarbonTax": carbontax,
    		//"InterestFactor":interestfactor,
    		"InternationalMarketPriceFactor":intmarketpricefactor,
    		"InternationalMarketLowestPriceApplied":intmarketlowestprice,
    		"AnnualCostFactor":timefactor
          }
          
  		query = JSON.stringify(query);        
      
      
      console.log("data of input= "+JSON.stringify(list)); 
        
      
      $.getJSON('/JPS_MEN/MENTableAgent',
    		  {
			  query
    		  },
               function(data) {
                    
               var result = data;
               console.log("data of result= "+JSON.stringify(result)); 
               $('#myProgressBar').remove()
               var n= $('#timefactor').val().replace("[","").replace("]","").split(","); 
               var totalyear=n.length;
                              
               var tablestring="<TABLE id=\"table\" border=\"2\" >";
                   
                          tablestring+="<tr>";
                          tablestring+="<TH>Project life times</TH>";
                          tablestring+="<TH>Transport Cost per year <br> (10^3 x $/yr)</TH>";
                          tablestring+="<TH> Material Purchase Cost per year <br> (10^9 x $/yr)</TH>";
                          tablestring+="<TH>Pipeline Installation Cost <br> (10^6 x $)</TH>";
                          tablestring+="<TH>CO<sub>2</sub> Emission per year <br> (ton/yr)</TH>";
                          tablestring+="<TH>CO<sub>2</sub> Emission cost per year <br> (10^3 x $/yr)</TH>";
                          for(var c=0; c<totalyear; c++) {
                        	  tablestring+="<TH>Total Cost Related to Transportation in "+n[c]+" year <br> (10^6 x $)</TH>";
                      			}
                          tablestring+="</tr>";
                          
                   for (var a=1; a <= totalyear; a++) { 
                	   tablestring+="<tr>";
                	   tablestring+="<TH>"+n[a-1]+" year</TH>";
                          for(var b=1; b<=5+totalyear; b++) {
                        	  tablestring+="<td align=\"center\" id=cell"+a+b+"></td>";
                      			}
                          tablestring+="</tr>";
                      }
                   tablestring+="</table>";	
                 document.getElementById("outputFields").innerHTML = tablestring; 
              
                   
                   for (var i=1; i<=timefactor.length;i++)
            	   {
               document.getElementById("cell"+i+"1").innerHTML = result.totalTransportationCost[i-1];
               document.getElementById("cell"+i+"2").innerHTML = result.totalMaterialPurchaseCost[i-1];
               document.getElementById("cell"+i+"3").innerHTML = result.totalInstallationCost[i-1];
               document.getElementById("cell"+i+"4").innerHTML = result.totalCO2Emission[i-1];
               document.getElementById("cell"+i+"5").innerHTML = result.totalCO2EmissionCost[i-1];
              
               		for(var h=0;h<timefactor.length;h++)
               			{
               			document.getElementById("cell"+i+String(6+h)).innerHTML = result.totaltransportyearcost[timefactor.length*(i-1)+h];
               			}
              			               			
            	   }
             
                })
                
                
    })
});
