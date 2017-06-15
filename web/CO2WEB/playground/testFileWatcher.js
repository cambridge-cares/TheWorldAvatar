/**
 * Created by Shaocong on 6/6/2017.
 */

var fs = require("fs");


console.log(__dirname+'/message.txt');
fs.watchFile(__dirname+'/message.txt', (curr, prev) => {
    console.log(`the current mtime is: ${curr.mtime}`);
    console.log(`the previous mtime was: ${prev.mtime}`);

    if(curr.mtime.getTime() === prev.mtime.getTime()){//time has truly elapsed
        //now you know file is changed
        //send the file directly to it


    }}
    );
