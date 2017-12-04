var runHeatWasteNetwork = require("../agents/RunHeatWasteNetwork")

runHeatWasteNetwork((err, result)=>{
    if(err) {
        console.log(err)
        next(err)
        return;
    }
    console.log(result)

    res.json(result);
})