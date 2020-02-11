/**
 * Created by MASTE on 7/7/2017.
 */

loadFrame(1,'data.tsv','graph1',"W/m^2" );
var refresh = self.setInterval("loadFrame(2,'data.tsv','graph1','W/m^2')",15000);


//loadFrame(1,'data2.tsv','graph2');
//var refresh2 = self.setInterval("loadFrame(2,'data2.tsv','graph2')",15000);


loadFrame(1,'data3.tsv','graph3','degree');
var refresh2 = self.setInterval("loadFrame(2,'data3.tsv','graph3','degree')",15000);



loadFrame(1,'data4.tsv','graph4','V');
var refresh3 = self.setInterval("loadFrame(2,'data4.tsv','graph4','V')",15000);

 
loadFrame(1,'data7.tsv','graph7','MW');
var refresh4 = self.setInterval("loadFrame(2,'data7.tsv','graph7','MW')",15000);



loadFrame(1,'data8.tsv','graph8','MVar');
var refresh5 = self.setInterval("loadFrame(2,'data8.tsv','graph8','MVar')",15000);






function loadFrame(index,filename,id,unit)
{
    if(index == 1)
    {

    }
    else
    {
        location.reload();
    }
var svg = d3.select("#" + id),
    margin = {top: 20, right: 20, bottom: 30, left: 50},
    width = +svg.attr("width") - margin.left - margin.right,
    height = +svg.attr("height") - margin.top - margin.bottom,
    g = svg.append("g").attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var parseTime = d3.timeParse("%H:%M");

var x = d3.scaleTime()
    .rangeRound([0, width]);

var y = d3.scaleLinear()
    .rangeRound([height, 0]);

 
	
var line = d3.line()
    .x(function(d) { return x(d.date); })
    .y(function(d) { return y(d.close); });
// "data.tsv"
d3.tsv(filename, function(d) {
    d.date = parseTime(d.date);
    d.close = +d.close;
    return d;
}, function(error, data) {
    if (error) throw error;

    x.domain(d3.extent(data, function(d) { return d.date; }));
    y.domain(d3.extent(data, function(d) { return d.close; }));


	
	
    g.append("g")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x))
        .select(".domain")
        .remove();

    g.append("g")
        .call(d3.axisLeft(y))
        .append("text")
        .attr("fill", "#000")
        .attr("transform", "rotate(-90)")
        .attr("y", 6)
        .attr("dy", "0.71em")
        .attr("text-anchor", "end")
        .text(unit + "");

	g.append("g")
      .attr("transform", "translate(0," + height + ")")
      .call(d3.axisBottom(x));
		
    g.append("path")
        .datum(data)
        .attr("fill", "none")
        .attr("stroke", "steelblue")
        .attr("stroke-linejoin", "round")
        .attr("stroke-linecap", "round")
        .attr("stroke-width", 1.5)
        .attr("d", line);
});
}