$( window ).on( "load", function() {
    console.log( "window loaded" );
//ajax to get co2 value from server


//set timer to do fluctuate
//
 var val =  parseFloat($("#co2Value").text());

window.setInterval(function () {
    setFluctuate(val)
}, 2000);



});

function setFluctuate(val){

    console.log(val);
    $("#co2Value").text(fluctuate(val));

}


function fluctuate(oldValue){

    let noise = generateGaussianNoise(0, 0.01);

    oldValue += noise;//add a random [-0.01,0.01)

    console.log("noise: "+noise);
    return oldValue.toFixed(4);

}
function generateGaussianNoise(mu, sigma){
    let generate = false;

    return GaussianNoise(mu, sigma);

    function GaussianNoise(mu, sigma){
        const epsilon  = Number.EPSILON;
        const twoPi = Math.PI * 2;

        let z0, z1;
        do{
            u1 = Math.random();
            u2 = Math.random();

        } while(u1 < epsilon);

        z0 = Math.sqrt(-2.0 * Math.log(u1)) * Math.cos(twoPi * u2);
        z1 = Math.sqrt(-2.0 * Math.log(u1)) * Math.sin(twoPi * u2);

        generate = !generate;

        if(!generate){
          return z1*sigma + mu;
        }

          return z0*sigma + mu;
    }



}


