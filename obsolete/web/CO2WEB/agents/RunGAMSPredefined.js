/**
 */
let csv = require('csv');
let parser = csv.parse();
const fs = require('fs')
function runGAMSPredefined(id, cb){
console.log("id: "+id)
 console.log(typeof  id)
    //params is predefined id
   let filename;
   switch(id){
       case 0:
           filename = "./agents/results.csv"
           break;
		   
	   case 1:
           filename = "./agents/results2.csv"
           break;   

     case 2:
          filename =   "./agents/results3.csv" 
           break;		  
       default:
           throw new Error("non-existing predefined resultset id");
   }
    console.log(filename)

   if(!filename){
       throw new Error("non-existing predefined resultset id")
   }

   console.log(filename)
 fs.readFile(filename, function (err ,rdata) {
     if(err){
         console.log(err)
         cb(err)
         return
     }
     csv.parse(rdata, function (err, data) {

         if(err){
             cb(err)
             console.log(err)
             return
         }
         console.log(data);
         data.splice(0,1)
         let result = data.map((item) =>{
             return {
                 uri:item[0],
                 capacity:item[1],
                 location:{lat:parseFloat(item[2]), lng:parseFloat(item[3])},
                 type: "nuclear"
                 }
         })

         console.log(result)

         cb(null ,result)
     })

 })





}

module.exports = runGAMSPredefined