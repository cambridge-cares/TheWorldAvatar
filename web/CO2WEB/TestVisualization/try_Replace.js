/**
 * Created by Shaocong on 9/5/2017.
 */
var path = require('path')
var url = "http://www.jparksimulator.com/Swanbank_B_Coal_Power_Plant_Australia.owl"
const ppRoot = path.join(__dirname, "powerplants");

url = url.replace("http://www.theworldavatar.com",ppRoot);
console.log(url)
url = url.replace("http://www.jparksimulator.com",ppRoot);
console.log(url)
