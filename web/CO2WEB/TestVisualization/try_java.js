/**
 * Created by Shaocong on 10/9/2017.
 */
var java = require("java");
java.classpath.push("javabuilder.jar");
java.classpath.push("HVAC_20170915.jar");
java.classpath.push("HVACCaller.jar");

//var HVACCaller = java.import("jparksim.sim.mau.hvaccaller.HvacCaller");
//console.log(HVACCaller);
let inputs = [23.0, 60.0,2.5e4,80.0,30.0];
//inputvalues = inputvalues.map((jsNumber)=>{return java.newDouble(jsNumber)})
console.log(java)
let result = java.callStaticMethodSync("jparksim.sim.mau.hvaccaller.HvacCaller", "callHVAC", inputs)

console.log(result)