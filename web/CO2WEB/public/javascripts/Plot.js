/**
 * Module that makes a page of plots that updating through socket events
 * @param opts
 * @constructor
 */


/**
 * A xy plot
 * @param opts
 * @constructor
 */
function Plot(opts) {
    console.log(opts)
    this.mergeOptions(opts);
};


Plot.prototype = {

    mergeOptions: function (opts) {
        this.parentEl = opts.parentEl || $(document.body);
        this.lineNum = opts.lineNum || 1;
        this.settings = {}
        this.settings.colors = opts.colors instanceof Array && opts.colors.length === opts.lineNum? opts.colors :(() => {
            let arr = [];
            let color = typeof opts.colors==="string"? opts.colors : "green";
            for (let i = 0; i < this.lineNum; i++) {
                arr.push(color);
            }
            return arr
        })();

        this.settings.scale = opts.scale || "linear";
        this.settings.showTitle = opts.showTitle || true;
        this.settings.names = opts.names instanceof Array && opts.names.length === opts.lineNum?opts.names : (() => {
                let arr = [];
                for (let i = 0; i < this.lineNum; i++) {
                    arr.push("Values = ");
                }
                return arr
            })();
    },
    initPlot : function (idataP) {
      let initDataObj = Object.assign({}, this.settings);
      console.log(initDataObj);
      initDataObj["yunit"] = idataP.unit;
        let valueSeries = idataP.value;
        initDataObj["start"] = valueSeries[0].time;
        initDataObj["end"] = valueSeries[valueSeries.length - 1].time;
        initDataObj["step"] = Math.round((initDataObj.end - initDataObj.start) / valueSeries.length);
        console.log(initDataObj["start"])
        console.log(initDataObj["end"])
        console.log(initDataObj["step"])

        initDataObj["values"] = [valueSeries.map((item) => {
            return typeof item.value === "string"?parseFloat(item.value): item.value;
        })];

        console.log(initDataObj);
       this.lineGraph = this.initSinglePlot(this.parentEl, idataP.name, initDataObj);//init this graph
        this.newDataObj = {
            "start": initDataObj["end"],
            "names" : ["value="]
        }
    },
    updatePlot : function (newDataP) {
        this.newDataObj.values = [];
        this.newDataObj.values[0] = [parseFloat(newDataP.value)];
        this.newDataObj["end"] =  newDataP.time;
        this.newDataObj["step"] = this.newDataObj["end"] - this.newDataObj["start"] ;//+ Math.round(Math.random()*500)???

        console.log("new value:" + JSON.stringify(this.newDataObj["values"]));
        console.log("step:" + this.newDataObj["step"]);
        console.log("end:" +  this.newDataObj["end"] );
        console.log("start:" +  this.newDataObj["start"] );

        this.lineGraph.slideData(this.newDataObj);
        this.newDataObj["start"]  = this.newDataObj["end"];
        console.log("updated next start:")
        console.log(this.newDataObj["start"]);

    },
    /**
     * Add a text title before plot
     * @param parentel
     * @param name
     */
    insertTitle : function (parentel, name) {
        let id = "title-"+name;
        parentel.append("<h2 class='plot-title' id='"+id+"'>"+name+"</h2>");
    },
    initSinglePlot : function (parentel,name, dataObj) {
        let id = "plot-"+name;

        if(this.settings.showTitle){
            this.insertTitle(parentel, name);
        }
        parentel.append("<p id='"+id+"'></p>");
        
        let el = $("#"+this.jqSelParse(id));
        console.log(el);
        el.css({
            position:"relative",
            height:"400px"
        });
        var l1 = new LineGraph({containerId: this.jqSelParse(id), data: dataObj});//init this graph
        console.log("finish initing new plot")
        return l1;
    },
    /**
     * Utility: underscore unaccepted chars for jquery selector
     * @param myid
     * @returns {void|XML|string|*}
     */
    jqSelParse: function(myid) {
    console.log(myid)
    return myid.replace(/(:|\.|\[|\]|,|=|@|\/)/g, "\\$1");
}


}

/**
 * A group of xy plots that could be updated together
 * @param opts
 * @constructor
 */
function MultiPlots(opts) {
    this.mergeOptions(opts);
    this.plotMap = {};
}


MultiPlots.prototype = {
    mergeOptions: function (opts) {
        this.parentEl = opts.parentEl || $(document.body);
        let settings = {};
        this.length = opts.length || 1;
        settings.parentEl = Array(this.length).fill(this.parentEl);
        console.log(settings.parentEl)
        settings.lineNums = opts.lineNums ||Array(this.length).fill(1);
        settings.colors = opts.colors && this.nestArrayLengthCheck(opts.colors, this.lineNums)?opts.colors : "green";//fine bc fall back, so dont worry;>
        settings.scale = opts.scale instanceof Array && opts.scales.length === this.length?opts.scale : "linear";
        settings.names = opts.names && this.nestArrayLengthCheck(opts.names, this.lineNums)? opts.names: "Value = " ;
        this.settingMap = []
        for(let i = 0; i < this.length; i++){
            let obj = {}
            for(let name in settings){
                obj[name] = typeof settings[name] === "string"? settings[name]:settings[name][0];
            }

           this.settingMap.push(obj)
        }
    },
    setSinglePlot : function (name,setting) {
        //pack settings
        let aPlot = new Plot(setting);
        this.plotMap[name] = aPlot;
        console.log("updated plot map:")
        console.log(this.plotMap)
        return aPlot;
    },

    getSinglePlot : function (name) {
        console.log("seraching for "+name)
      return this.plotMap[name]
    },
    initPlots : function (datas) {
        for(let i = 0; i < datas.length; i++){
            let datum = datas[i];
            console.log(datum)
            let plot = this.setSinglePlot(datum.name, this.settingMap[i]);
            plot.initPlot(datum);
        }
    },
    updatePlots : function (data) {

        data.forEach((datum)=>{
            let plot= this.getSinglePlot(datum.name);
            if(!plot){
                console.log("can not find plot: "+datum.name)
                return;
            }
            console.log(datum)
            let valueSeries = datum.value
            plot.updatePlot(datum.value[valueSeries.length-1]);
        });


    },
    nestArrayLengthCheck : function (narr2c, lengths) {
      if(!narr2c || narr2c.length !== sarr.length){
          return false;
      }
      for(let i= 0; i < narr2c.length; i++){
          if(narr2c[i].length !== lengths[i]){
              return false;
          }
      }
      return true;
      }


}