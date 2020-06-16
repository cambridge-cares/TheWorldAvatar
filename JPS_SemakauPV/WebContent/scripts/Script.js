/**
 * Created by Laura Ong 12/2/2020
 */

//
//loadFrame(1, 'data.tsv', 'graph1', "W/m^2");
//var refresh = self.setInterval("loadFrame(2,'data.tsv','graph1','W/m^2')", 15000);
//
//
////loadFrame(1,'data2.tsv','graph2');
////var refresh2 = self.setInterval("loadFrame(2,'data2.tsv','graph2')",15000);
//
//
//var refresh2 = self.setInterval("loadFrame(2,'data3.tsv','graph3','degree')", 15000);
//
//
//loadFrame(1, 'data4.tsv', 'graph4', 'V');
//var refresh3 = self.setInterval("loadFrame(2,'data4.tsv','graph4','V')", 15000);
//
//
//loadFrame(1, 'data7.tsv', 'graph7', 'MW');
//var refresh4 = self.setInterval("loadFrame(2,'data7.tsv','graph7','MW')", 15000);
//
//
//loadFrame(1, 'data8.tsv', 'graph8', 'MVar');
//var refresh5 = self.setInterval("loadFrame(2,'data8.tsv','graph8','MVar')", 15000);

var firstQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " +
	"PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> " +
	"PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> " +
	"PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> " +
	"PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> " +
	"PREFIX j6:<http://www.w3.org/2006/time#> " +
	"PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> " +
	"PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> " +
	"SELECT ?VoltMagvalue ?VoltAnglevalue ?proptimeval ?BaseKVvalue "

	+
	"WHERE {?entity  a  j1:BusNode  ." +
	"?entity   j2:isModeledBy ?model ."

	+
	"?model   j5:hasModelVariable ?VM ." +
	"?VM  a  j3:Vm  ." +
	"?VM  j2:hasValue ?vVM ." +
	"?vVM   j2:numericalValue ?VoltMagvalue ." // Vm
	+
	" ?vVM   j6:hasTime ?proptime ." +
	" ?proptime   j6:inXSDDateTime ?proptimeval ."

	+
	"?model   j5:hasModelVariable ?VA ." +
	"?VA  a  j3:Va  ." +
	"?VA  j2:hasValue ?vVA ." +
	"?vVA   j2:numericalValue ?VoltAnglevalue ." // Va
	+
	" ?vVA   j6:hasTime ?proptime ." +
	" ?proptime   j6:inXSDDateTime ?proptimeval ."

	+
	"?model   j5:hasModelVariable ?BKV ." +
	"?BKV  a  j3:baseKV  ." +
	"?BKV  j2:hasValue ?vBKV ." +
	"?vBKV   j2:numericalValue ?BaseKVvalue ." // Base KV
	+
	"}" +
	"ORDER BY ASC(?proptime)";
var secondQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " +
	"PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> " +
	"PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> " +
	"PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> " +
	"PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> " +
	"PREFIX j6:<http://www.w3.org/2006/time#> " +
	"PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> " +
	"PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> " +
	"SELECT ?activepowervalue ?reactivepowervalue ?proptimeval "

	+
	"WHERE {?entity  a  j1:PowerGenerator  ." +
	"?entity   j2:isModeledBy ?model ."

	+
	"?model   j5:hasModelVariable ?Pg ." +
	"?Pg  a  j3:Pg  ." +
	"?Pg  j2:hasValue ?vpg ." +
	"?vpg   j2:numericalValue ?activepowervalue ." // pg
	+
	" ?vpg   j6:hasTime ?proptime ." +
	" ?proptime   j6:inXSDDateTime ?proptimeval ."


	+
	"?model   j5:hasModelVariable ?Qg ." +
	"?Qg  a  j3:Qg  ." +
	"?Qg  j2:hasValue ?vqg ." +
	"?vqg   j2:numericalValue ?reactivepowervalue ." // qg
	+
	" ?vqg   j6:hasTime ?proptime ." +
	" ?proptime   j6:inXSDDateTime ?proptimeval ." +
	"}" +
	"ORDER BY ASC(?proptime)";

var irradiationQuery = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
	+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
	+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
	+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
	+ "SELECT ?entity ?propval ?proptimeval "
	+ "WHERE { ?entity a j5:Q-Sensor ." 
	+ "  ?entity j4:observes ?prop ." 
	+ " ?prop   j2:hasValue ?vprop ."
	+ " ?vprop   j2:numericalValue ?propval ." 
	+ " ?vprop   j6:hasTime ?proptime ."
	+ " ?proptime   j6:inXSDDateTime ?proptimeval ." 
	+ "}" 
	+ "ORDER BY ASC(?proptimeval)";
var prefix = "http://www.jparksimulator.com";
var bus="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/EBus-006.owl";
var gen="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-002.owl";
var irradiationSensor = "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
function main(){
	console.log(bus);
	console.log(gen);
	console.log(firstQuery);
	var kmlurl = createUrlForSparqlQuery("base", bus, firstQuery);
	var request = $.ajax({
	    url: kmlurl,
	    type: 'GET',
	    contentType: 'application/json; charset=utf-8',
	    success: function(){ 
	    },
	    error: function(ts) {
	        alert(ts.responseText);
	    }   
	});
	kmlurl = createUrlForSparqlQuery("base", gen, secondQuery);
	var request2 = $.ajax({
	    url: kmlurl,
	    type: 'GET',
	    contentType: 'application/json; charset=utf-8',
	    success: function(){ 
	    },
	    error: function(ts) {
	        alert(ts.responseText);
	    }   
	});
	kmlurl = createUrlForSparqlQuery("base", irradiationSensor, irradiationQuery);
	var request3 = $.ajax({
	    url: kmlurl,
	    type: 'GET',
	    contentType: 'application/json; charset=utf-8',
	    success: function(){ 
	    },
	    error: function(ts) {
	        alert(ts.responseText);
	    }   
	});
	request3.done(function(data){
		var obj0 = JSON.parse(data);
        obj0 = obj0['results']['bindings'];
        propvallst = [];
        propTime = [];
        for (var i in obj0){
        	propvallst.push(obj0[i].propval.value);
        	propTime.push(obj0[i].proptimeval.value.split('T')[1].split('+')[0]);
        }
        makeChart(propvallst, propTime , 'graph1', "W/m^2", 'rgba(211, 181, 146, 1)');
	});
	request.done(function(data){
		var obj0 = JSON.parse(data);
        obj0 = obj0['results']['bindings'];
        VoltMagvaluelst = [];
        VoltAnglevaluelst = [];
        propTime = [];
        for (var i in obj0){
        	VoltMagvaluelst.push(obj0[i].VoltMagvalue.value);
        	VoltAnglevaluelst.push(obj0[i].VoltAnglevalue.value);
        	propTime.push(obj0[i].proptimeval.value.split('T')[1].split('+')[0]);
        }

        makeChart(VoltAnglevaluelst, propTime , 'graph3', 'degree');
        makeChart(VoltMagvaluelst, propTime , 'graph4', 'V');
	});
	request2.done(function(data){
		var obj0 = JSON.parse(data);
        obj0 = obj0['results']['bindings'];
        activepowervaluelst = [];
        reactivepowervaluelst = [];
        propTime = [];
        for (var i in obj0){
        	activepowervaluelst.push(obj0[i].activepowervalue.value);
        	reactivepowervaluelst.push(obj0[i].reactivepowervalue.value);
        	console.log(obj0[i].proptimeval.value);
        	propTime.push(obj0[i].proptimeval.value.split('T')[1].split('+')[0]);
        }
        makeChart(activepowervaluelst, propTime , 'graph7', 'MW');
        makeChart(reactivepowervaluelst, propTime , 'graph8', 'Mvar');
	});
	
}
function createUrlForSparqlQuery(scenarioname, iri, sparql) {

	var url2 = prefix + '/jps/scenario/base/query?query=';
	urljson = {
		"scenarioresource": iri,
		"sparqlquery": sparql
	};
	url2 += encodeURIComponent(JSON.stringify(urljson));
	//url2 += JSON.stringify(urljson); 
	return url2;
}

function makeChart(dataset, time, id, unit){
	 makeChart(dataset, time, id, unit,"rgba(39, 211, 122, 1)");
}
function makeChart(dataset, time, id, unit, color){
	var v = dataset;
	console.log(time);
	v.forEach(function(obj){obj = parseFloat(obj)});
	vGraph = new Chart(id, {
		type: 'line', 
		data: {
			labels: time, 
			datasets:[
				{	
					label:'',
					pointRadius: 2, 
					pointHoverRadius:5, 
					backgroundColor:color,
		            data: v, 
		            fill: false,
				}
				]
			}, 
		options:{
			title:{ 
		        text: unit,
		        display: true
			}
		}, 
		responsive:true,
		maintainAspectRatio:true

	})
}

main();
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