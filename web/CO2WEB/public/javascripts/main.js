/**
 * Created by Shaocong on 4/5/2017.
 */
$( window ).on( "load", function() {
    console.log( "window loaded" );
//ajax to get co2 value from server


//set timer to do fluctuate

window.setInterval(setFluctuate, 2000);



});

function setFluctuate(){

    var val =  parseFloat($("#co2Value").text());
    console.log(val);
    $("#co2Value").text(fluctuate(val));

}


function fluctuate(oldValue){


    oldValue +=Math.random()*20 - 10;//add a random [-10,10)

    return oldValue.toFixed(4);

}

