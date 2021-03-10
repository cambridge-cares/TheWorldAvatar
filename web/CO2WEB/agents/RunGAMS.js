



//TODO:Initial data from owl that would not change anyway at this moment, lazy init
//TODO: user inputs as parameters, write into the input file
//TODO: run a child process of MAU
//TODO :read result from output file

/***
executed on the GAMS python file
***/
function runGams(){
    //read
    function runCMD(program, params){
        var spawn = require('child_process').spawn,
        p    = spawn('python', params);
        return p;
    }











}


