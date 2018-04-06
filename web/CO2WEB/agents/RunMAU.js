/**
This modules runs MAU, which is a jar.
 There should be a future unifed module that wraps around the java module and eases the use of runing any jar file.
 */
//use npm java module to run the function
var path = require('path')
var java = require("java");
java.classpath.push(path.resolve("./agents/javabuilder.jar"));
java.classpath.push(path.resolve("./agents/HVAC_20170915.jar"));
java.classpath.push(path.resolve("./agents/HVACCaller.jar"));
function RunMAU(){

   // this.setLibrary();


}


RunMAU.prototype = {

    setLibrary : function () {


    },
    runSimulation: function (inputs) {

        if(!this.checkInputs(inputs)){
         console.log("err : not all inputvalues are numeric")
            return null;
        }
        console.log("REsults: ");


        return java.callStaticMethodSync("jparksim.sim.mau.hvaccaller.HvacCaller", "callHVAC", inputs);
    },

    checkInputs :function (inputs) {
if(inputs.length < 5){
    console.log("err: input length smaller taht 5")
    return false;
}
        return inputs.every((item)=>{
            return this.isNumeric(item);
        })

    },

    isNumeric :function (n) {
        return !isNaN(parseFloat(n)) && isFinite(n);
    }


};

/**
function test() {
    let MAU = new RunMAU()
let inputvalues = [23, 60,25000,80,30];

MAU.runSimulation(inputvalues)
}
**/


module.exports = RunMAU;





