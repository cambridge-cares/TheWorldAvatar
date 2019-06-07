/**
 */

//Concentration contour map threshould number
const THRESHOULD_NUM = 8

//Height is faked, each plane displayed between an interval
const HEIGHT_INTERVAL = 5
const LEGEND_DIV = 'legendwrapper'



function getContourMaps(address,folder) {
    
    return new Promise((resolve, reject) => {
        
        $.ajax({
        	url: address,
        	data: {
        		folder: folder
        	},
        	dataType: 'text'
        })//todo: change to actual endpoint in future,
        .done(function (d2result) {
                console.log('get contour data')
                  d2result = JSON.parse(d2result)
            
               // d2result = [ [d2result,d2result],[d2result,d2result], [d2result,d2result]]
                //heights=   d2result.heights = [1.1,2.2,3.3,4.4,5.5]
    
                //=============contour consts======================//
				                d2result = d2Arr21d(d2result);//to 1d

				let ubound  =  d2arraymax(d2result);
				console.log('ub:'+enlarge(ubound))
                
                  const thresholdsC = d3.scaleLog() // [0, 1, 2, 3, 4, 5, 6, 7, 8]
                    .domain([1, enlarge(ubound)]).range([0, THRESHOULD_NUM]);
				
					let ticks = numberarray(THRESHOULD_NUM+1);
					console.log(ticks)
					console.log(thresholdsC.invert(3))
					let  thresholds = ticks.map((tik)=> {return thresholdsC.invert(tik)});
										let  thresholdsO = thresholds.map((t)=> restore(t));

					console.log(thresholds)
                const middle = thresholds[Math.floor(thresholds.length / 2)];
                const color = d3.scaleLog(d3.interpolateRdYlBu)
                    .domain([1, middle, d3.max(thresholds)])
                    .range(['#3986ce', '#fee08b', '#d73027'])
                    .interpolate(d3.interpolateLab);
                
                //=============legend ======================//
                
                let legendWrapperNode = document.getElementById("legendwrapper");
                while(legendWrapperNode.firstChild) {
                	legendWrapperNode.removeChild(legendWrapperNode.firstChild);
                }
                makeLegend(LEGEND_DIV, thresholds, color,thresholdsO);
                
                //=============contour map as svg for each  ======================//
                
                let svgstrs = d2result.map((output) => {
                    var svg = d3.select("#svgwrapper svg"),
                        width = +svg.attr("width"),
                        height = +svg.attr("height");
                    //todo:
                    let ROW_NUM = 80;
                    let COL_NUM = 80;
                    let values = output;
                    
					values = values.map((v)=> enlarge(v))
                    let contours = d3.contours()
                        .size([COL_NUM, ROW_NUM])
                        .thresholds(thresholds);
                    
                    
                    svg.selectAll("path")
                        .data(contours(values))
                        .enter().append("path")
                        .attr("d", d3.geoPath(d3.geoIdentity().scale(width / COL_NUM)))
                        .attr("fill", function (d) {
                            return color(d.value);
                        });
                    let svgstr = $('#svgwrapper').html();
                    $('#svgwrapper svg').empty();//clear up and start again
                    return svgstr
                    
                });
                
                
                //=========set up canvas for image conversion=========================//
               
                // var newCanvas = $('<canvas/>', {'id': 'drawcanvas'})
                //    .width(2048)//todo: customize size
                 //   .height(2048);
                //$('body').append(newCanvas);
                var canvas = $('#drawcanvas')[0];
                var context = canvas.getContext('2d');
    			
                context.translate(canvas.width, 0);
				context.scale(-1,1);

                //========convert all svg strs to png images=============//
                
				let futureImages = svgstrs.map((svgstr) => {
                    return svgToImagePromise(svgstr)
                })
                
                //======parse all image to dataurls=====================/
                
                Promise.all(futureImages).then(images => {
                    let dataurls = images.map((image) => {

                        context.fillStyle = "white";

                        context.fillRect(0, 0, canvas.width, canvas.height);
                        context.drawImage(image, 0, 0)
                        let dataurl = context.canvas.toDataURL('image/png')
                       context.clearRect(0, 0, canvas.width, canvas.height);
                        return dataurl
                    });
                    console.log(dataurls)
                    
                    
                    resolve(dataurls)
                    
                }, err => {//todo: err handling
                    reject(err)
                });
                
                
                //todo: this is the command for actualluy draw on map
                //osmb.addGeoJSON(geojson,{ elevation: HEIGHT_INTERVAL * ++numCount, hasTexture:dataurl});
                
                
            })
            .fail(function (err) {
                    //todo: err handling
                    reject(err)
                }
            )
        
    })
    
    
}

function numberarray(len){
	var arr = [];

for(var i = 0; i < len; i++){
    arr.push(i);
}
return arr;
}
function enlarge(v){
	return Math.sqrt(v)*10000;
}

function restore(v){
	let t = v/10000;
	
	return t*t*t;
}

function d2arraymax(d2array){
	let mmax = 0;
	    d2array.forEach((arr) =>{
        let arrmax = Math.max(...arr);
		if(arrmax > mmax) {mmax = arrmax}
    })
	return mmax

}

function svgToImagePromise(svgstr) {
    return new Promise((resolve, reject) => {
        svgToImage(svgstr, function (err, image) {
            if (err) {
                reject(err);
                return;
            }
            resolve(image)
        })
    });
    
}

function d2Arr21d(d2array){
    coned = []
    d2array.forEach((row) =>{
        coned = coned.concat(row)
    })
    return coned;
    
}

function makeLegend(selector_id, thresholds, color, thresholdsO) {
    var thresholdScale = d3.scaleThreshold()
        .domain(thresholdsO)
        .range(["rgb(244, 244, 244)"].concat(d3.range(THRESHOULD_NUM)
     .map(function(i) { return color(thresholds[i])})));

	 
    console.log(d3.range(THRESHOULD_NUM+1).map(function(i) { return color(thresholds[i])}))
    console.log(d3.range(6).map(function(i) { return "q" + i + "-9"}))

    var svg = d3.select("#"+selector_id )
        .append("svg")
		.style("height", "155px")
		.style("width", "160px")
  console.log(svg);
    
    svg.append("g")
        .attr("class", "legendQuant");
       // .attr("transform", "translate(20,20)");
    console.log('addl legend')
    var legend = d3.legendColor()
        .labelFormat(d3.format(".2e"))
        .labels(d3.legendHelpers.thresholdLabels)
        .scale(thresholdScale)
    
    svg.select(".legendQuant")
        .call(legend);
		
	$("text.label").append(" \u03BCg/m\u00B3"); // micrograms/meter^3
}

function makeSlider(selector_id, levelnum, callback){
    var parent = $("#"+selector_id );
    console.log(parent)
    parent.append('<p><label for="amount">Height:</label><input class="readonlyinput" type="text" id="height-show" readonly></p>')

    var slider = $( "<div id='slider'></div>" ).appendTo( parent ).slider({
        min: 0,
        max: levelnum - 1,
        range: "min",
        value: 0,
        orientation: "vertical",
    
        slide: callback
    });
    $( "#minbeds" ).on( "change", function() {
        slider.slider( "value", this.selectedIndex + 1 );

    });
    
}

function makeRadios(selector_id,list, legend){
    var set = $("<form class='radiogroup'></form>").appendTo($("#"+selector_id ));
    if(legend){
        set.append("<legend>"+legend+"</legend>")
    }
    list.forEach((item)=>{
        set.append($("<input type='radio'  value ='"+item+"' name='radio' >"+"<label>"+item+"&nbsp;</label>"))
    })
    
      console.log(set.children('input')[0])
    $('.radiogroup').children('input').first().attr('checked', true);
    
}

//***********This part is main call*****************************//

//make radio group

const POL_LIST = ["CO2"   ,"CO" ,  "NO2" ,  "HC" ,  "NOx", "Particulate001"]
const POL_NUM =POL_LIST.length
const HEIGHT_NUM =4

