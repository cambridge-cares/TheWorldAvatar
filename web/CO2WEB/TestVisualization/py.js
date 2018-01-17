fs = require('fs')

fs.readdir('../testFiles', function (err, files) {
    console.log(files)
})
