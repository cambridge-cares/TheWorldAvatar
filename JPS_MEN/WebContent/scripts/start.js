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
 // If start fails (ie simulation doesn't work anymore, replace the method with this:)
    $('#startv2').click(function(){
		document.getElementById("outputFields").innerHTML = '<div id="outputFields"><table id="table" border="2"><tbody><tr><th>Project life times</th><th>Transport Cost per year <br> (10^3 x $/yr)</th><th> Material Purchase Cost per year <br> (10^9 x $/yr)</th><th>Pipeline Installation Cost <br> (10^6 x $)</th><th>CO<sub>2</sub> Emission per year <br> (ton/yr)</th><th>CO<sub>2</sub> Emission cost per year <br> (10^3 x $/yr)</th><th>Total Cost Related to Transportation in 1 year <br> (10^6 x $)</th><th>Total Cost Related to Transportation in 10 year <br> (10^6 x $)</th><th>Total Cost Related to Transportation in 50 year <br> (10^6 x $)</th><th>Total Cost Related to Transportation in 100 year <br> (10^6 x $)</th></tr><tr><th>1 year</th><td id="cell11" align="center">577.38</td><td id="cell12" align="center">6.42</td><td id="cell13" align="center">0.00</td><td id="cell14" align="center">802.84</td><td id="cell15" align="center">40.14</td><td id="cell16" align="center">0.58</td><td id="cell17" align="center">5.77</td><td id="cell18" align="center">28.87</td><td id="cell19" align="center">57.74</td></tr><tr><th>10 year</th><td id="cell21" align="center">64.49</td><td id="cell22" align="center">6.64</td><td id="cell23" align="center">0.68</td><td id="cell24" align="center">108.33</td><td id="cell25" align="center">5.42</td><td id="cell26" align="center">0.74</td><td id="cell27" align="center">1.32</td><td id="cell28" align="center">3.90</td><td id="cell29" align="center">7.13</td></tr><tr><th>50 year</th><td id="cell31" align="center">22.54</td><td id="cell32" align="center">6.64</td><td id="cell33" align="center">1.55</td><td id="cell34" align="center">53.71</td><td id="cell35" align="center">2.69</td><td id="cell36" align="center">1.57</td><td id="cell37" align="center">1.78</td><td id="cell38" align="center">2.68</td><td id="cell39" align="center">3.80</td></tr><tr><th>100 year</th><td id="cell41" align="center">12.66</td><td id="cell42" align="center">6.64</td><td id="cell43" align="center">2.39</td><td id="cell44" align="center">40.85</td><td id="cell45" align="center">2.04</td><td id="cell46" align="center">2.40</td><td id="cell47" align="center">2.52</td><td id="cell48" align="center">3.02</td><td id="cell49" align="center">3.66</td></tr></tbody></table></div>'; 
	})
});
