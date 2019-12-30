/***
Implements the Sim prototype. for Mau.
***/
const toggleDisplay = elemId =>
{
    let x = document.getElementById(elemId);
    if (x.style.display !== 'block')
    {
        x.style.display = 'block';
    }
    else
    {
        x.style.display = 'none';
    }
};
$("#readme-button").click(function ()
{
    toggleDisplay("readme-text");
});
document.addEventListener("click", function (evt){
    var readmeButtonElement = document.getElementById('readme-button'),
        readmeTextElement = document.getElementById('readme-text'),
        targetElement = evt.target; // clicked element
    if (targetElement == readmeButtonElement || targetElement == readmeTextElement)
    {
        return; //readme-button or readme-text is clicked. do nothing.
    }
    
});
  addWeatherData();
  d3.csv("images/weather.csv").then(makeChart);
  var time;
  function makeChart(data){
    //data is an array of objects where each object represents a datapoint
    time = data.map(function(d){return d.Time});
    var temperature = data.map(function(d){return d.AirTemp})
    tempGraph = new Chart("temperature", {
      type: 'bar',
      data: {
        labels:time,
        datasets: [
          {
          backgroundColor:'rgba(126, 158, 211, 1)',
          data: temperature, 
          }
        ]
      }, 
      options:{
        legend:{
          display:false
        },
        scales:{
          xAxes:[{
            ticks:{
              fontSize:16
            }
          }]
        },
        tooltips:{
          titleFontSize: 36,
          bodyFontSize: 24, 
          callbacks:{
            label: function(tooltipItems, data){
              return tooltipItems.yLabel + "\xB0C";
            }
          }
        },
        title:{
          fontSize:48, 
          text: "Temperature", 
          display: true
        },
        responsive: true,
        maintainAspectRatio: true
      }
    });
    
    var irradiation = data.map(function(d){return d.IncomingRadiation})
    irradGraph = new Chart("irradiation", {
      type: 'line',
      data: {
        labels:time,
        datasets: [
          {
            pointRadius:10,
            pointHoverRadius:11,
            backgroundColor:'rgba(211, 181, 146, 1)',
            data: irradiation, 
          }
        ]
      }, 
      options:{
        legend:{
          display:false
        },
        scales:{
          yAxes:[{
            type:'linear',
            ticks: {
              fontSize:16
         }
          }], 
          xAxes:[{
            ticks:{
              fontSize:16
            }
          }]
        },
        tooltips:{
          titleFontSize: 36,
          bodyFontSize: 24
        },
        title:{
          fontSize:48, 
          text:"Solar Radiation",
          display: true
        },
        responsive: true,
        maintainAspectRatio: true
      }
    });
  }
  function addWeatherData(){
    var weatherjson = {};
    weatherjson["tempsensor"] = "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001";
    weatherjson["speedsensor"]="http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001";
    weatherjson["irradiationsensor"]="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
    weatherjson["jpscontext"]= "base";
    var weatherurl = "http://localhost:8080/JPS_DES/GetIrradiationandWeatherData"  + "?query=" + encodeURIComponent(JSON.stringify(weatherjson));
    console.log(weatherurl);
    var request = $.ajax({
      url: "http://localhost:8080/JPS_DES/GetIrradiationandWeatherData",
      type: 'GET',
      data: weatherjson,
      contentType: 'application/json; charset=utf-8'
  });

  request.done(function(data) {
    console.log(data);
    radiation = JSON.parse(data).irrad;
    temp = JSON.parse(data).temperature;
    addData(tempGraph, temp);
    addData(irradGraph, radiation);
    irradGraph.data.labels.pop();
    irradGraph.update();
    removeData(tempGraph);
    setTimeout(function() {
        console.log('timeout');  
        irradGraph.data.datasets[0].data.shift();
        irradGraph.update();
    }, 2000);
  //   setTimeout(function() {
  //     console.log('timeout');
  //     removeLine(irradGraph); 
  // }, 10000);
  });
  }

  //add data totgenbu
  function addData(chart, elem){
    chart.data.labels.push(new Date().toLocaleTimeString());
    console.log(elem);
    chart.data.datasets[0].data.push(elem);
    chart.update();
  }
  function removeData(chart) {
    chart.data.labels.shift();
    chart.data.datasets[0].data.shift();
    chart.update();
}
function removeLine(chart) { //for line since there seems to be an error
  chart.data.labels.pop();
  chart.data.datasets[0].data.shift();
  chart.update();
}
  //Outputs to be taken from csv here. 
  d3.csv("images/totgenbu.csv").then(makeOutputChart);
  function makeOutputChart(data){
    cA = data.map(function(d){return d.cA});
    cB = data.map(function(d){return d.cB});
    cC = data.map(function(d){return d.cC});
    cD = data.map(function(d){return d.cD});
    console.log(cA);
    console.log(cB);
    console.log(cC);
    console.log(cD);
    var chart = new Chart("graph4", {
      type: 'line',
      data: {
        labels:time,
        datasets: [
          {
            label:'Home',
            pointRadius:10,
            pointHoverRadius:11,
            borderColor:"rgba(39, 211, 122, 1)",
            data: cA, 
            fill: false,
          }, 
          {
            label:'Industrial',
            pointRadius:10,
            pointHoverRadius:11,
            borderColor:"rgba(85, 71, 189, 1)",
            data: cB, 
            fill: false,
          },  {
            label:'Commercial',
            pointRadius:10,
            pointHoverRadius:11,
            borderColor:"rgba(186, 34, 191, 1)",
            data: cC, 
            fill: false,
          },  {
            label:'Fuel Cell',
            pointRadius:10,
            pointHoverRadius:11,
            borderColor:"rgba(221, 44, 127, 1)",
            data: cD, 
            fill: false,
          }
        ]
      }, 
      options:{
        legend:{
          display:true
        },
        scales:{
          yAxes:[{
            type:'linear',
            ticks: {
              fontSize:16
         }
          }], 
          xAxes:[{
            ticks:{
              fontSize:16
            }
          }]
        },
        tooltips:{
          titleFontSize: 36,
          bodyFontSize: 24
        },
        title:{
          fontSize:48, 
          text:"Power Load",
          display: true
        },
        responsive: true,
        maintainAspectRatio: true
      }
    });
  
  }
  
