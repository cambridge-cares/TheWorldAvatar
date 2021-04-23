/***
Implements the Sim prototype. for Mau.
***/
console.log('desplot live find')
var prefix = "http://localhost:8080";
// prefix = "http://jparksimulator.com";
ENIRI="http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
DISIRI="http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
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
  // addWeatherData();
  var hourOfDay = [];
  
  function makeChart(data){
    //data is an array of objects where each object represents a datapoint
    console.log(hourOfDay);
    var temperature = data.temperature;
    temperature.forEach(function(obj) { obj = parseFloat(obj)});
    console.log(temperature)
    tempGraph = new Chart("temperature", {
      type: 'bar',
      data: {
        labels:hourOfDay,
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
    
    var irradiation = data.irradiation;
    
    irradiation.forEach(function(obj) { obj = parseFloat(obj)});
    irradGraph = new Chart("irradiation", {
      type: 'line',
      data: {
        labels:hourOfDay,
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
          text:"Solar Irradiance",
          display: true
        },
        responsive: true,
        maintainAspectRatio: true
      }
    });
  }
  //run AddWeatherData upon activation)
  function addWeatherData(){
      document.getElementById("loader").style.display = "block";
    var request = $.ajax({
      url: prefix + "/JPS_DES/showDESResult",
      type: 'GET',
      timeout:1.08e+7,
      contentType: 'application/json; charset=utf-8'
  });

  request.done(function(data) {
    console.log(data);
    response = JSON.parse(data);
    hourOfDay = response.timer;
    makeChart(response);
    makeOutputChart(response);
    configRH("rh1", response.rh1);
    configRH("rh2", response.rh2);
    configRH("rh3", response.rh3);
    displayHash(response.txHash, response.sandr);
    document.getElementById("loader").style.display = "none";
  });

  }
  
  function displayHash(data, sandr){
    console.log(data);
    var lst = document.getElementById("txhashes");
    var le = [];
    for (let x in data){
      console.log(x + " : "+data[x]);
      var li = document.createElement("li");
      li.appendChild(document.createTextNode(data[x] + "\t" + sandr[x]));
      li.addEventListener("click", function(){
        window.open( "https://rinkeby.etherscan.io/tx/"+data[x], "_blank");
      }, false);
      lst.appendChild(li);
      le.push(li);
    }
  }

 
function configRH(idofgraph, data){
  
  data[0].forEach(function(obj) { obj = parseFloat(obj)});
  
  data[1].forEach(function(obj) { obj = parseFloat(obj)});
  
  data[2].forEach(function(obj) { obj = parseFloat(obj)});
  console.log(data[0])
  
  console.log(data[1])
  
  console.log(data[2])
  var chart = new Chart(idofgraph, {
    type: 'line',
    data: {
      labels:hourOfDay,
      datasets: [
        {
          label:'Appliance Load',
          pointRadius:2,
          pointHoverRadius:5,
          borderColor:"#0ff253",
          data: data[0], 
          fill: false,
        }, 
        {
          label:'Battery Load',
          pointRadius:2,
          pointHoverRadius:5,
          borderColor:"#f1c40f",
          data: data[1], 
          fill: false,
        },  {
          label:'Total Power Consumption',
          pointRadius:2,
          pointHoverRadius:5,
          borderColor:"#f20fae",
          data: data[2], 
          fill: false,
        }
      ]
    },
    options:{
      title:{ 
        text:idofgraph,
        display: true
      }
    },
    responsive: true,
    maintainAspectRatio: true
  })
}
  function makeOutputChart(data){
    cA = data.residential;
    cB = data.commercial;
    cC = data.industrial;
    cD = data.solar;
    cE = data.gridsupply;
    
    cA.forEach(function(obj) { obj = parseFloat(obj)});
    cB.forEach(function(obj) { obj = parseFloat(obj)});
    cC.forEach(function(obj) { obj = parseFloat(obj)});
    cD.forEach(function(obj) { obj = parseFloat(obj)});
    cE.forEach(function(obj) { obj = parseFloat(obj)});

    outputGraph = new Chart("graph4", {
      type: 'line',
      data: {
        labels:hourOfDay,
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
            label:'Commercial',
            pointRadius:10,
            pointHoverRadius:11,
            borderColor:"rgba(85, 71, 189, 1)",
            data: cB, 
            fill: false,
          },  {
            label:'Industrial',
            pointRadius:10,
            pointHoverRadius:11,
            borderColor:"rgba(186, 34, 191, 1)",
            data: cC, 
            fill: false,
          },  {
            label:'Renewable',
            pointRadius:10,
            pointHoverRadius:11,
            borderColor:"rgba(221, 44, 127, 1)",
            data: cD, 
            fill: false,
          } ,
          {
            label:'Grid Supply',
            pointRadius:10,
            pointHoverRadius:11,
            borderColor:'#d8de2c',
            data: cE, 
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
  addWeatherData()    