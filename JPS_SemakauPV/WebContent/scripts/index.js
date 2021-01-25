$(document).ready(function(){
	
	//-----------------------------------------------------------------------------------//
	
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
    
    //-----------------------------------------------------------------------------------//

	$.getJSON('/JPS_SemakauPV/SemakauVisualization',
		{
			pvgenerator:"http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-002.owl#PV-002",
			ebus:"http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/EBus-006.owl#EBus-006",
			irradiationsensor:"http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001"
		},
		function(data){
			console.log(data);
			let propvallst = data.propVal;
			let propTime = data.proptime;
			makeChart(propvallst, propTime , 'graph1', "W/m^2", 'rgba(211, 181, 146, 1)');
			
			let VoltAnglevaluelst= data.VoltAng;
			let VoltMagvaluelst = data.VoltMag;
			makeChart(VoltAnglevaluelst, propTime , 'graph3', 'degree');
       		makeChart(VoltMagvaluelst, propTime , 'graph4', 'V');
			let reactivepowervaluelst= data.reactivePower;
			let activepowervaluelst = data.activePower;
			makeChart(activepowervaluelst, propTime , 'graph7', 'MW');
        	makeChart(reactivepowervaluelst, propTime , 'graph8', 'Mvar');
		}).fail( function(){
			alert("Connection to server failed");
		})
	function makeChart(dataset, time, id, unit){
		makeChart(dataset, time, id, unit,"rgba(39, 211, 122, 1)");
	}
	function makeChart(dataset, time, id, unit, color){
		var v = dataset;
		console.log(time);
		v.forEach(function(obj){obj = parseFloat(obj)});
		vGraph = new Chart(id, {
			type: 'line', 
			data: {
				labels: time, 
				datasets:[
					{	
						label:'',
						pointRadius: 2, 
						pointHoverRadius:5, 
						backgroundColor:color,
			            data: v, 
			            fill: false,
					}
					]
				}, 
			options:{
				title:{ 
			        text: unit,
			        display: true
				}
			}, 
			responsive:true,
			maintainAspectRatio:true
	
		})
	}

});