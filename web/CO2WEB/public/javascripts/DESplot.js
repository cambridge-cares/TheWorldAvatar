

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
$(document).ready(function ()
{
  d3.csv("images/weather.csv").then(makeChart);
  var time;
  function makeChart(data){
    //data is an array of objects where each object represents a datapoint
    time = data.map(function(d){return d.Time});
    var temperature = data.map(function(d){return d.AirTemp})
    var chart = new Chart("temperature", {
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
        maintainAspectRatio: false
      }
    });
    
    var irradiation = data.map(function(d){return d.IncomingRadiation})
    var chart = new Chart("irradiation", {
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
            type:'logarithmic',
            ticks: {
              min: 0,
              max: 1000,
              callback: function (value, index, values) {
                  if (value === 1000) return "1K";
                  if (value === 100) return "100";
                  if (value === 10) return "10";
                  if (value === 0) return "0";
                  return null;
              }
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
        maintainAspectRatio: false
      }
    });
  }



  //Outputs to be taken from csv here. 
  
  d3.csv("images/totgenbu.csv").then(makeOutputChart);
  function makeOutputChart(data){
    capacity = data.map(function(d){return d.capacity});
    var chart = new Chart("graph4", {
      type: 'line',
      data: {
        labels:time,
        
        datasets: [
          {
            pointRadius:10,
            pointHoverRadius:11,
            borderColor:'rgba(146, 212, 146, 1)',
            data: capacity, 
          }
        ]
      }, 
      options:{
        legend:{
          display:false
        },
        scales:{
          yAxes:[{
            type:'logarithmic',
            ticks: {
              fontSize:16,
              min: 0,
              max: 1000,
              callback: function (value, index, values) {
                  if (value === 1000) return "1K";
                  if (value === 100) return "100";
                  if (value === 10) return "10";
                  if (value === 0) return "0";
                  return null;
              }
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
          text:"Home",
          display: true
        },
        responsive: true,
        maintainAspectRatio: false
      }
    });
  
  }
})
