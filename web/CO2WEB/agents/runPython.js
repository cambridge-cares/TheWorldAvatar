/**
 * This is a common module for running python script.
 * It is yet quite primitive and might needs future calibration.
 */
function runPython(filename, args, cb) {
    var spawn = require('child_process').spawn,
        params = args? [filename, ...args] : [filename]
        py    = spawn('python', params),
     dataStr  = '';

    py.stdout.on('data', handleStdout);
    py.stderr.on('data', handleStdout);

    py.on('error', handleErr);

    py.on('exit', handleExit);

   // py.stdin.write(JSON.stringify(args));
    //py.stdin.end();


    //out format-use json

    function handleErr(err) {
        console.log("ERR!!!!:")
        console.log(err)
    }
    function handleStdout(data) {
        console.log("RAW Datastr:")
        console.log(data.toString());
        let jsonReg = /JSON({.*})/;
        let matches = jsonReg.exec(data.toString())
        if(matches && matches.length>0) {
            console.log(matches);
            dataStr += matches[1];
            console.log("TRIMED Datastr:")
            console.log(dataStr)
        }
    };

    function handleExit(code, signal) {
        console.log('child process exited with code ' + code+" signal: "+signal);
        py.kill()
        console.log("final data str:")
        console.log(dataStr)
        if(dataStr.length > 0){
            try{
            cb(null, JSON.parse(dataStr));

            }catch(err){
              cb(err);
            }

        } else {
            cb(new Error("got no datastr from py exe:"+filename))
        }
    };
}





module.exports = runPython;



