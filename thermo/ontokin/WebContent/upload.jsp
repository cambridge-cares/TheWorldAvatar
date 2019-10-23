<?xml version="1.0" encoding="UTF-8" ?>
<%@ page contentType="text/html; charset=UTF-8"%>
<%@ taglib prefix="s" uri="/struts-tags"%>
<%@ taglib prefix="sj" uri="/struts-jquery-tags"%>
<%@ taglib prefix="sb" uri="/struts-bootstrap-tags"%>

<!DOCTYPE html>
<html>
<!--  after pressing refresh button it clears content of page. -->
<head>

<meta charset="UTF-8">
<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
<script src="${pageContext.request.contextPath}/js/Chart.min.js"></script>
<script src="${pageContext.request.contextPath}/js/utils.js"></script>
<script src="${pageContext.request.contextPath}/js/popper.min.js"></script>
<script src="${pageContext.request.contextPath}/js/bootstrap.min.js"></script>
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/bootstrap.min.css">
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/index.css">

<link rel="icon"
	href="${pageContext.request.contextPath}/css/static/group/favicon.ico" />

<link rel="stylesheet" type="text/css"
	href="${pageContext.request.contextPath}/css/static/group/ontokin.css">

<title>J-Park Simulator</title>

</head>

<body>
<div class="jumbotron text-center" id="topBanner">
    <a id="cares-banner" href="http://www.cares.cam.ac.uk">
        <img id="care-banner-img" src="images/cam_lang_negativ1%20NEW_0.png">
    </a>
		<h1 id="title">OntoKin</h1>
		<p id="description">
			A Knowledge Base built with the integration of semantic technologies and software agents for 
			enhancing the experience of chemists in querying chemical kinetic reaction 
			mechanisms. This User Interface (UI) demonstrates the query capability of OntoKin.
		</p>
</div>
	<div class="container">
		<div class="row">
             <div class="col-md-3 div-left">
             
                     <s:actionerror />
			<s:actionmessage />			
		
			<s:form action="upload" method="post"
				enctype="multipart/form-data" theme="bootstrap" label="Select CHEMKIN files to upload:">
				<hr class="line">
				<s:file name="myMechFile" label="Select a mechanism file to upload:"
					theme="bootstrap" />
				<s:file name="myThermoFile" label="Select a thermo chemistry file to upload:"
					theme="bootstrap" />
				<s:file name="mySurfaceFile" label="Select a surface chemistry file to upload:"
					theme="bootstrap" />
				<s:file name="myTransportFile" label="Select a transport file to upload:"
					theme="bootstrap" />								
				<s:textfield name="myMechanismName" type="text" label="Provide a name for the mechanism:" theme="bootstrap"/>
				<s:submit value="Upload" label="Select files" theme="bootstrap" />
			</s:form>
			
		<p></p>				
			
		<s:iterator value="column" var="c">
			<hr class="line">
			<legend>Mechanism upload report </legend>
			<table class="table table-bordered table-hover">
				<tr>
					<th>Item</th>
					<th>Result</th>
				</tr>
				<tr>
					<td ><s:property value="column1" /></td>
					<td><s:property value="myMechanismName" /></td>
				</tr>
				<tr>
					<td ><s:property value="column2" /></td>
					<td><s:property value="myMechFileFileName" /></td>
				</tr>
				<tr>
					<td ><s:property value="column3" /></td>
					<td><s:property value="myThermoFileFileName" /></td>
				</tr>
				<tr>
					<td ><s:property value="column4" /></td>
					<td><s:property value="mySurfaceFileFileName" /></td>
				</tr>
				<tr>
					<td ><s:property value="column5" /></td>
					<td><s:property value="myTransportFileFileName" /></td>
				</tr>
				<tr>
					<td ><s:property value="column6" /></td>
					<td><s:property value="myChemkinValidationReport" /><br></br>Please see the conversion log <a href="<s:property value="myChemkinValidationReportFile" />">here.</a><br></br>If the conversion fails, check line(s) starting with ERROR for potential reasons. Please contact Prof. Markus Kraft(mk306@cam.ac.uk) and Dr. Jethro Akroyd (jwja2@cam.ac.uk) for reporting any issue.</td>
				</tr>
					<tr>
					<td ><s:property value="column7" /></td>
					<td><s:property value="myOWLConsistencyReport" /></td>
				</tr>
			</table>
			
		</s:iterator>	
		</div>
		
			<div class="col-md-half">
			</div>
			<div class="col-md-eightpointfive div-right">
				<s:actionerror/>
				<legend style="padding-bottom: 10px;">Specify a Query:</legend>
				<hr class="line !important;">
				<span id ="queryType" style="">Select a type of query:</span>
				<span id ="errorType" style="display:none; color:red">No type selected</span>
				<div class="row">
					<div class="col-md-11">
						<s:select
							headerKey="-1" headerValue="Select query type"
							list="#{'mechAll':'Show All Mechanisms (no additional input is needed)', 'mechforS':'Show Mechanism(s) Containing Species', 'thermo':'Thermodynamic Data', 'compthermo':'Compare Thermodyanmic Data', 'mechforR':'Show Mechanism(s) Containing Reaction', 'rateconstant':'Show Arrhenius Rate Constant Parameters (Matching Reaction Exactly)', 'comparerate':'Compare Arrhenius Rate Constant Parameters (Matching Reaction Exactly)', 'rateconstantAnyOrder':'Show Arrhenius Rate Constant Parameters (Matching Reactants and Products, may take several minutes)', 'comparerateAnyOrder':'Compare Arrhenius Rate Constant Parameters (Matching Reactants and Products, may take several minutes)'}" 
							name="querySelection" 
							value="thermo" theme="bootstrap" />
					</div>
					<div class="col-md-1">
						<span class="btn btn-sm btn-info btn-help" data-toggle="tooltip" data-placement="right" title="To view all the mechanisms, select 'Show All Mechanisms'. To search for a species, select either 'Show Mechanism(s) Containing Species' or 'Thermodynamic Data' or 'Compare Thermodynamic Data'. To search for a reaction, select either 'Show Arrhenius Rate Constant Parameters' or 'Compare Arrhenius Rate Constant Parameters'.">?</span>							
					</div>
					<div class="col-md-11">
						<span id ="units" style="">Select the Units of R (gas constant):</span>
						<s:select
							headerKey="-1" headerValue="kcalmolk':'kcal / mol. K"
							list="#{'jmolk':'J / mol. K', 'ergmolk':'erg / mol. K', 'dimensionless':'Dimensionless'}" 
							name="unitsSelection" 
							value="unitsR" theme="bootstrap" />
					</div>
				</div>
				<span id ="queryText" style="">Interactive text:</span>
				<span id ="errorQuery" style="display:none; color:red">No text provided</span>
				<div class="row">
					<div class="col-md-11">
						<s:textfield name="term" class="form-control"  placeholder="Specify the name of a species or reaction" theme="bootstrap"/>							
					</div>
					<div class="col-md-1">
						<span class="btn btn-sm btn-info btn-help" data-toggle="tooltip" data-placement="right" title="If you have selected 'Show All Mechanisms' in the menu above, click on the 'Search OntoKin' button. If you have selected a species related query, type in or paste a species (e.g. H2O). If you have selected a reaction related query, type in or past a reaction (e.g. O + HO2 [=] O2 + OH). Finally, click on the 'Search OntoKin' button.">?</span>
					</div>
				</div>

					<%-- <s:submit id="search_btn" value="OntoKin Search (to view the list of all mechanisms in the KB, go directly to the following drop-down menu)" theme="bootstrap"/> --%>
					<button id="execute" theme="bootstrap">Search OntoKin</button>
					<button id="refresh" theme="bootstrap">Clear</button>
					<span id="spinner" style="display: none"><i class="fa fa-spinner fa-spin" style="font-size:24px"></i></span>
					
					<p></p><span id ="noResult" style="display:none; color:red">No results found.</span>
				 	<p></p>
				 	
				 	<!-- Instruction -->
					<div class="row">
					 	<div class="col-md-12">
						 	<div id="accordion">
							  <div class="card">
							    <div class="card-header" id="headingOne">
							      <h5 class="mb-0">
							        <button class="btn btn-link" data-toggle="collapse" data-target="#collapseOne" aria-expanded="true" aria-controls="collapseOne">
									    How to search?
							        </button>
							      </h5>
							    </div>
							
							    <div id="collapseOne" class="collapse show" aria-labelledby="headingOne" data-parent="#accordion">
							      <div class="card-body">
										<h5 class="card-title" style="font-size: 18px;">How to search for a species?</h5>
									    <p class="card-text" style="font-size: 14px;">In the 'OntoKin Search' box above, provide a species (e.g. CH4) by typing in or pasting, select either 'Show Mechanism(s) Containing Species' or 'Thermodynamic Data' or 'Compare Thermodynamic Data' in the 'Select query type' drop-down menu and click on the 'Search OntoKin' button.</p>
									    <h5 class="card-title" style="font-size: 18px;">How to search for a reaction?</h5>
									    <p class="card-text" style="font-size: 14px;">In the 'OntoKin Search' box above, provide a reaction (e.g. OH [=] H2O + O) by typing in or pasting, select either 'Show Mechanism(s) Containing Reaction' or 'Show Arrhenius Rate Constant Parameters' or 'Compare Arrhenius Rate Constant Parameters' in the 'Select query type' drop-down menu and click on the 'Search OntoKin' button.</p>
									</div>
							    </div>
							  </div>
							</div>
						  </div>
						</div>
					
					<!-- Mechanism Table -->
				 	<div id="tableMechanism" class="Container Flipped">
					 	<div id="table" class="Content">
					 		<table class="table table-bordered table-hover">
					 			<thead>
					    			<tr class="row-header">
					    			</tr>
					   			</thead>
					   			<tbody id="table-query-results">
					   			</tbody>
					   		</table>
					   	</div>
				   	</div>
				  
				  <!-- Draw Charts -->
					<div class="container chart-group">
					  <div class="row">
					   	<div id="chartCanvas" class="" style="display:none">
							<canvas id="canvas" style="width:800px !important; height:500px"></canvas>
						</div>
					   	<div id="chartCanvasRateAE" class="" style="display:none">
							<canvas id="canvasRateAE" style="width:800px !important; height:300px"></canvas>
							<p></p>
						</div>
					   	<div id="chartCanvasRatePEF" class="" style="display:none">
							<canvas id="canvasRatePEF" style="width:800px !important; height:300px"></canvas>
							<p></p>
						</div>
					   	<div id="chartCanvasRateTE" class="" style="display:none">
							<canvas id="canvasRateTE" style="width:800px !important; height:300px"></canvas>							
						</div>
					  </div>
					</div>
					</div>
		</div>
	</div>


<script type="text/javascript">

$( function() {
 
	 function splitString(string) {
		  	if (string.indexOf('[=]') > -1) {
		    	return string.split("[=]");
		    } else if (string.indexOf('=]') > -1) {
		    	return string.split("=]");
		    } else if (string.indexOf('<=>')){
		    	return string.split("<=>");
		    } else if (string.indexOf('=>')){
		    	return string.split("=>");
		    }
		  }
	 
	 function formatLabel(str){
		if(str.length>30){
			str = str.slice(0,30) + '...';
		}
		return str;
	}
	
	$("a#linkButton").on('click',function(){
	    window.open('www.google.com', '_blank');
	});
	
	$('[data-toggle="tooltip"]').tooltip({
	    trigger : 'hover'
	});  
	
	let getTableResultRowString = (index, resultObj) => {
		let tdNodes = '';
		
		//console.log(resultObj);
		for (let x in resultObj) {
			tdNodes += '<td>' + resultObj[x] + '</td>';
		}
		
		return '<tr class="row-query-results"> ' +
		    		'<td class="index">' + index + '</td>' +
	    		 
	    		tdNodes +
				'</tr>'
	};
	
	$('#refresh').click(function() {
		window.location.href = '/ontokin';
	});

	$("#execute").on("click", (event) => {		
		
		// let chartCanvas =  $("#chartCanvas");
		let queryString;
		
		let search_term_name = $("#term").val(); //cl2
		let search_querySelection = $("#querySelection").val(); //thermo
	
		$("#errorQuery").hide();
		$("#errorType").hide();
		$("#noResult").hide();
		if (search_querySelection != 'mechAll' && (search_term_name.trim() ==  '' || search_querySelection ==  -1 || search_querySelection ==  undefined || search_querySelection ==  null)) {
			if(search_term_name ==  '') {
				$("#chartCanvas").hide();
				$("#chartCanvasRateAE").hide();
				$("#chartCanvasRatePEF").hide();
				$("#chartCanvasRateTE").hide();				
				$("#tableMechanism").hide();
				$("#errorQuery").show();
			}
			if(search_querySelection ==  -1 || search_querySelection ==  undefined || search_querySelection ==  null) {
				$("#chartCanvas").hide();
				$("#chartCanvasRateAE").hide();
				$("#chartCanvasRatePEF").hide();
				$("#chartCanvasRateTE").hide();				
				$("#tableMechanism").hide();
				$("#errorType").show();
			}			
			event.preventDefault(); 
		}
		else {
			search_term_name = search_term_name.toUpperCase();
			let result = splitString(search_term_name);			
			let reactantArray;
			let productArray;
			
			if (result.length == 2 ) {	
				reactantArray = result[0].split("+");
				productArray = result[1].split("+");
			}
			
			$("#errorQuery").hide();
			$("#errorType").hide();
			$("#noResult").hide();
		
			if (search_querySelection == 'mechAll') {			 
				 
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'SELECT ?MechanismName ?MechanismIRI' + '\n' +
				'WHERE {' + '\n' +
				    '?MechanismIRI rdf:type ontokin:ReactionMechanism .' + '\n' +
				    '?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
				'} ';
								
			} else if (search_querySelection == 'mechforS' && !(search_term_name.indexOf('=') > -1)) {			 
			 
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'SELECT ?MechanismName ?MechanismIRI' + '\n' +
				'WHERE {' + '\n' +
					'?Species rdfs:label \"' + search_term_name + '\" . ?Species ontokin:belongsToPhase ?Phase . ?Phase ontokin:containedIn ?MechanismIRI .'+ '\n' +
				    '?MechanismIRI rdf:type ontokin:ReactionMechanism .' + '\n' +
				    '?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
				'} ';
								
			} else if (search_querySelection == 'mechforR' && search_term_name.indexOf('=') > -1) {
			 
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'SELECT ?MechanismName ?MechanismIRI' + '\n' +
				'WHERE {' + '\n' +
					'?Reaction ontokin:hasEquation \"' + search_term_name + '\" . ?Reaction ontokin:belongsToPhase ?Phase . ?Phase ontokin:containedIn ?MechanismIRI .'+ '\n' +
				    '?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
				'} ';
							
			} else if(search_querySelection == 'thermo') {
			 
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'SELECT ?MechanismName ?MechanismIRI ?SpeciesIRI ?ThermoModelIRI ?CoefficientValues ?MinTemp ?MaxTemp' + '\n' +
				'WHERE {' + '\n' +
					'?SpeciesIRI rdfs:label \"' + search_term_name + '\" . ?SpeciesIRI ontokin:belongsToPhase ?Phase . ?Phase ontokin:containedIn ?MechanismIRI .'+ '\n' +
					'?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
					'?MechanismIRI rdf:type ontokin:ReactionMechanism .' + '\n' +
				    '?SpeciesIRI ontokin:hasThermoModel ?ThermoModelIRI .' + '\n' +
				    '?ThermoModelIRI ontokin:hasCoefficientValues ?CoefficientValues .' + '\n' +
				    '?ThermoModelIRI ontokin:hasMinimumTemperature ?MinTemp .' + '\n' +
				    '?ThermoModelIRI ontokin:hasMaximumTemperature  ?MaxTemp .' + '\n' +
				'} ';
				
			} else if(search_querySelection == 'compthermo') {
			 
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'SELECT ?MechanismName ?MechanismIRI ?SpeciesIRI ?ThermoModelIRI ?CoefficientValues ?MinTemp ?MaxTemp ?Pressure' + '\n' +
				'WHERE {' + '\n' +
					'?SpeciesIRI rdfs:label \"' + search_term_name + '\" . ?SpeciesIRI ontokin:belongsToPhase ?Phase . ?Phase ontokin:containedIn ?MechanismIRI .'+ '\n' +
				    '?MechanismIRI rdf:type ontokin:ReactionMechanism .' + '\n' +
				    '?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
				    '?SpeciesIRI ontokin:hasThermoModel ?ThermoModelIRI .' + '\n' +
				    '?ThermoModelIRI ontokin:hasCoefficientValues ?CoefficientValues .' + '\n' +
				    '?ThermoModelIRI ontokin:hasMinimumTemperature ?MinTemp .' + '\n' +
				    '?ThermoModelIRI ontokin:hasMaximumTemperature  ?MaxTemp .' + '\n' +
				    '?ThermoModelIRI ontokin:hasPressure  ?Pressure .' + '\n' +
	 				'}';
			} else if(search_querySelection == 'rateconstant') {
			 
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'SELECT ?MechanismName ?MechanismIRI ?ReactionIRI ?ActivationEnergy ?ActivationEnergyUnits ?PreExpFactor ?PreExpFactorUnits ?TempExponent ?TempExpUnits' + '\n' +
				'WHERE {' + '\n' +
					'?ReactionIRI ontokin:hasEquation \"' + search_term_name + '\" . ?ReactionIRI ontokin:belongsToPhase ?Phase . ?Phase ontokin:containedIn ?MechanismIRI .'+ '\n' +
			    	'?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
				    '?ReactionIRI ontokin:hasArrheniusCoefficient ?ArrheniusRateCoefficients .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergy ?ActivationEnergy .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergyUnits ?ActivationEnergyUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactor ?PreExpFactor .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactorUnits ?PreExpFactorUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponent ?TempExponent .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponentUnits ?TempExpUnits .' + '\n' +
 				'}';

			} else if(search_querySelection == 'comparerate') {
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'SELECT ?MechanismName ?MechanismIRI ?ReactionIRI ?ActivationEnergy ?ActivationEnergyUnits ?PreExpFactor ?PreExpFactorUnits ?TempExponent ?TempExpUnits' + '\n' +
				'WHERE {' + '\n' +
					'?ReactionIRI ontokin:hasEquation \"' + search_term_name + '\" . ?ReactionIRI ontokin:belongsToPhase ?Phase . ?Phase ontokin:containedIn ?MechanismIRI .'+ '\n' +
			    	'?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
				    '?ReactionIRI ontokin:hasArrheniusCoefficient ?ArrheniusRateCoefficients .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergy ?ActivationEnergy .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergyUnits ?ActivationEnergyUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactor ?PreExpFactor .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactorUnits ?PreExpFactorUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponent ?TempExponent .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponentUnits ?TempExpUnits .' + '\n' +
 				'}';
			} else if(search_querySelection == 'rateconstantAnyOrder' && reactantArray.length == 2 && productArray.length == 2) {
			 
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'PREFIX reaction_mechanism: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>' + '\n' +
				'SELECT ?MechanismName ?MechanismIRI ?ReactionIRI ?ActivationEnergy ?ActivationEnergyUnits ?PreExpFactor ?PreExpFactorUnits ?TempExponent ?TempExpUnits' + '\n' +
				'WHERE {' + '\n' +
		           '?ReactionIRI reaction_mechanism:hasProduct ?Product1 .' + '\n' + 
		              	   '?Product1 owl:sameAs ?Species1 .' + '\n' +
		            	   '?Species1 rdfs:label \"' + productArray[0].trim() + '\" .' + '\n' +  
		                   '?ReactionIRI reaction_mechanism:hasProduct ?Product2 .' + '\n' +
		              	   '?Product2 owl:sameAs ?Species2 .' + '\n' +
		              	   '?Species2 rdfs:label \"' + productArray[1].trim() + '\" .' + '\n' + 
		                   '?ReactionIRI reaction_mechanism:hasReactant ?Reactant1 .' + '\n' +
		              	   '?Reactant1 owl:sameAs ?Species3 .' + '\n' + 
		            	   '?Species3 rdfs:label \"' + reactantArray[0].trim() + '\" .' + '\n' + 
		                   '?ReactionIRI reaction_mechanism:hasReactant ?Reactant2 .' + '\n' +
		              	   '?Reactant2 owl:sameAs ?Species4 .' + '\n' + 
		              	   '?Species4 rdfs:label \"' + reactantArray[1].trim() + '\" .' + '\n' +
					'?ReactionIRI ontokin:belongsToPhase ?Phase . ?Phase ontokin:containedIn ?MechanismIRI .' + '\n' +
			    	'?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
				    '?ReactionIRI ontokin:hasArrheniusCoefficient ?ArrheniusRateCoefficients .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergy ?ActivationEnergy .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergyUnits ?ActivationEnergyUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactor ?PreExpFactor .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactorUnits ?PreExpFactorUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponent ?TempExponent .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponentUnits ?TempExpUnits .' + '\n' +
 				'}';
 				
 				console.log("querystring:\n"+queryString);
 				console.log("\n reactant1:\n"+reactantArray[0].trim());
 				console.log("\n reactant2:\n"+reactantArray[1].trim());
 				console.log("\n product1:\n"+productArray[0].trim());
 				console.log("\n product2:\n"+productArray[1].trim());

			} else if(search_querySelection == 'rateconstantAnyOrder' && reactantArray.length == 1 && productArray.length == 2) {
			 
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'PREFIX reaction_mechanism: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>'+ '\n' +
				'SELECT ?MechanismName ?MechanismIRI ?ReactionIRI ?ActivationEnergy ?ActivationEnergyUnits ?PreExpFactor ?PreExpFactorUnits ?TempExponent ?TempExpUnits' + '\n' +
				'WHERE {' + '\n' +
		                   '?ReactionIRI reaction_mechanism:hasProduct ?Product1 .'+ '\n' + 
		              	   '?Product1 owl:sameAs ?Species1 .'+ '\n' +
		            	   '?Species1 rdfs:label \"' + productArray[0].trim() + '\" .'+ '\n' +  
		                   '?ReactionIRI reaction_mechanism:hasProduct ?Product2 .'+ '\n' +
		              	   '?Product2 owl:sameAs ?Species2 .'+ '\n' +
		              	   '?Species2 rdfs:label \"' + productArray[1].trim() + '\" .'+ '\n' + 
		                   '?ReactionIRI reaction_mechanism:hasReactant ?Reactant1 .'
		              	   '?Reactant1 owl:sameAs ?Species3 .'+ '\n' + 
		            	   '?Species3 rdfs:label \"' + reactantArray[0].trim() + '\" .'+ '\n' + 
					'?ReactionIRI ontokin:belongsToPhase ?Phase . ?Phase ontokin:containedIn ?MechanismIRI .'+ '\n' +
			    	'?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
				    '?ReactionIRI ontokin:hasArrheniusCoefficient ?ArrheniusRateCoefficients .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergy ?ActivationEnergy .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergyUnits ?ActivationEnergyUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactor ?PreExpFactor .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactorUnits ?PreExpFactorUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponent ?TempExponent .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponentUnits ?TempExpUnits .' + '\n' +
 				'}';

			} else if(search_querySelection == 'rateconstantAnyOrder' && reactantArray.length == 2 && productArray.length == 1) {
			 
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'PREFIX reaction_mechanism: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>'+ '\n' +
				'SELECT ?MechanismName ?MechanismIRI ?ReactionIRI ?ActivationEnergy ?ActivationEnergyUnits ?PreExpFactor ?PreExpFactorUnits ?TempExponent ?TempExpUnits' + '\n' +
				'WHERE {' + '\n' +
		           		   '?ReactionIRI reaction_mechanism:hasProduct ?Product1 .'+ '\n' + 
		              	   '?Product1 owl:sameAs ?Species1 .'+ '\n' +
		            	   '?Species1 rdfs:label \"' + productArray[0].trim() + '\" .'+ '\n' +  
		                   '?ReactionIRI reaction_mechanism:hasReactant ?Reactant1 .'
		              	   '?Reactant1 owl:sameAs ?Species3 .'+ '\n' + 
		            	   '?Species3 rdfs:label \"' + reactantArray[0].trim() + '\" .'+ '\n' + 
		                   '?ReactionIRI reaction_mechanism:hasReactant ?Reactant2 .'+ '\n' +
		              	   '?Reactant2 owl:sameAs ?Species4 .'+ '\n' + 
		              	   '?Species4 rdfs:label \"' + reactantArray[1].trim() + '\" .'+ '\n' +
					'?ReactionIRI ontokin:belongsToPhase ?Phase . ?Phase ontokin:containedIn ?MechanismIRI .'+ '\n' +
			    	'?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
				    '?ReactionIRI ontokin:hasArrheniusCoefficient ?ArrheniusRateCoefficients .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergy ?ActivationEnergy .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergyUnits ?ActivationEnergyUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactor ?PreExpFactor .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactorUnits ?PreExpFactorUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponent ?TempExponent .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponentUnits ?TempExpUnits .' + '\n' +
 				'}';

			} else if(search_querySelection == 'comparerateAnyOrder' && reactantArray.length == 2 && productArray.length == 2) {
			 
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'PREFIX reaction_mechanism: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>' + '\n' +
				'SELECT ?MechanismName ?MechanismIRI ?ReactionIRI ?ActivationEnergy ?ActivationEnergyUnits ?PreExpFactor ?PreExpFactorUnits ?TempExponent ?TempExpUnits' + '\n' +
				'WHERE {' + '\n' +
		           '?ReactionIRI reaction_mechanism:hasProduct ?Product1 .' + '\n' + 
		              	   '?Product1 owl:sameAs ?Species1 .' + '\n' +
		            	   '?Species1 rdfs:label \"' + productArray[0].trim() + '\" .' + '\n' +  
		                   '?ReactionIRI reaction_mechanism:hasProduct ?Product2 .' + '\n' +
		              	   '?Product2 owl:sameAs ?Species2 .' + '\n' +
		              	   '?Species2 rdfs:label \"' + productArray[1].trim() + '\" .' + '\n' + 
		                   '?ReactionIRI reaction_mechanism:hasReactant ?Reactant1 .' + '\n' +
		              	   '?Reactant1 owl:sameAs ?Species3 .' + '\n' + 
		            	   '?Species3 rdfs:label \"' + reactantArray[0].trim() + '\" .' + '\n' + 
		                   '?ReactionIRI reaction_mechanism:hasReactant ?Reactant2 .' + '\n' +
		              	   '?Reactant2 owl:sameAs ?Species4 .' + '\n' + 
		              	   '?Species4 rdfs:label \"' + reactantArray[1].trim() + '\" .' + '\n' +
					'?ReactionIRI ontokin:belongsToPhase ?Phase . ?Phase ontokin:containedIn ?MechanismIRI .' + '\n' +
			    	'?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
				    '?ReactionIRI ontokin:hasArrheniusCoefficient ?ArrheniusRateCoefficients .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergy ?ActivationEnergy .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergyUnits ?ActivationEnergyUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactor ?PreExpFactor .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactorUnits ?PreExpFactorUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponent ?TempExponent .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponentUnits ?TempExpUnits .' + '\n' +
 				'}';
 				
 				console.log("querystring:\n"+queryString);
 				console.log("\n reactant1:\n"+reactantArray[0].trim());
 				console.log("\n reactant2:\n"+reactantArray[1].trim());
 				console.log("\n product1:\n"+productArray[0].trim());
 				console.log("\n product2:\n"+productArray[1].trim());

			} else if(search_querySelection == 'comparerateAnyOrder' && reactantArray.length == 1 && productArray.length == 2) {
			 
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'PREFIX reaction_mechanism: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>'+ '\n' +
				'SELECT ?MechanismName ?MechanismIRI ?ReactionIRI ?ActivationEnergy ?ActivationEnergyUnits ?PreExpFactor ?PreExpFactorUnits ?TempExponent ?TempExpUnits' + '\n' +
				'WHERE {' + '\n' +
		                   '?ReactionIRI reaction_mechanism:hasProduct ?Product1 .'+ '\n' + 
		              	   '?Product1 owl:sameAs ?Species1 .'+ '\n' +
		            	   '?Species1 rdfs:label \"' + productArray[0].trim() + '\" .'+ '\n' +  
		                   '?ReactionIRI reaction_mechanism:hasProduct ?Product2 .'+ '\n' +
		              	   '?Product2 owl:sameAs ?Species2 .'+ '\n' +
		              	   '?Species2 rdfs:label \"' + productArray[1].trim() + '\" .'+ '\n' + 
		                   '?ReactionIRI reaction_mechanism:hasReactant ?Reactant1 .'
		              	   '?Reactant1 owl:sameAs ?Species3 .'+ '\n' + 
		            	   '?Species3 rdfs:label \"' + reactantArray[0].trim() + '\" .'+ '\n' + 
					'?ReactionIRI ontokin:belongsToPhase ?Phase . ?Phase ontokin:containedIn ?MechanismIRI .'+ '\n' +
			    	'?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
				    '?ReactionIRI ontokin:hasArrheniusCoefficient ?ArrheniusRateCoefficients .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergy ?ActivationEnergy .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergyUnits ?ActivationEnergyUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactor ?PreExpFactor .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactorUnits ?PreExpFactorUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponent ?TempExponent .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponentUnits ?TempExpUnits .' + '\n' +
 				'}';

			} else if(search_querySelection == 'comparerateAnyOrder' && reactantArray.length == 2 && productArray.length == 1) {
			 
				queryString = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>' + '\n' +
			 	'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>' + '\n' + 
				'PREFIX ontokin:' + '\n' +
				'<http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>'+ '\n' +
				'PREFIX reaction_mechanism: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>'+ '\n' +
				'SELECT ?MechanismName ?MechanismIRI ?ReactionIRI ?ActivationEnergy ?ActivationEnergyUnits ?PreExpFactor ?PreExpFactorUnits ?TempExponent ?TempExpUnits' + '\n' +
				'WHERE {' + '\n' +
		           		   '?ReactionIRI reaction_mechanism:hasProduct ?Product1 .'+ '\n' + 
		              	   '?Product1 owl:sameAs ?Species1 .'+ '\n' +
		            	   '?Species1 rdfs:label \"' + productArray[0].trim() + '\" .'+ '\n' +  
		                   '?ReactionIRI reaction_mechanism:hasReactant ?Reactant1 .'
		              	   '?Reactant1 owl:sameAs ?Species3 .'+ '\n' + 
		            	   '?Species3 rdfs:label \"' + reactantArray[0].trim() + '\" .'+ '\n' + 
		                   '?ReactionIRI reaction_mechanism:hasReactant ?Reactant2 .'+ '\n' +
		              	   '?Reactant2 owl:sameAs ?Species4 .'+ '\n' + 
		              	   '?Species4 rdfs:label \"' + reactantArray[1].trim() + '\" .'+ '\n' +
					'?ReactionIRI ontokin:belongsToPhase ?Phase . ?Phase ontokin:containedIn ?MechanismIRI .'+ '\n' +
			    	'?MechanismIRI rdfs:label ?MechanismName .' + '\n' +
				    '?ReactionIRI ontokin:hasArrheniusCoefficient ?ArrheniusRateCoefficients .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergy ?ActivationEnergy .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasActivationEnergyUnits ?ActivationEnergyUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactor ?PreExpFactor .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasPreExponentialFactorUnits ?PreExpFactorUnits .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponent ?TempExponent .' + '\n' +
				    '?ArrheniusRateCoefficients ontokin:hasTemperatureExponentUnits ?TempExpUnits .' + '\n' +
 				'}';

			}
//		}
		
		let queryResultsTable = $("#table-query-results");
		$("#num-results").text("");
		$(".row-query-results").remove();
		
		$("#spinner").show();
		$.ajax({
			type: 'GET',
			//url: "http://www.theworldavatar.com/OntoKinGUI/OntoKinEndpointProxy",
			url: "http://localhost:8080/OntoKinGUI/OntoKinEndpointProxy",
			data: {queryString},
			success: data => {
				let trimmedResult = data.slice(1, data.length-2);
				let resultArray = trimmedResult.split('}');
				if (resultArray.length == 1) {
					$("#chartCanvas").hide();
					$("#chartCanvasRateAE").hide();
					$("#chartCanvasRatePEF").hide();
					$("#chartCanvasRateTE").hide();						
					$("#tableMechanism").hide();
					$("#noResult").show();
				} else {
					resultArray.pop();
					
					firstResult = resultArray[0] + '}';
					firstResult = firstResult.replace(/\n +/g, "");
					firstResultObj = JSON.parse(firstResult);
					$(".row-header").append(`<th class="row-query-results first-column">Index</th>`);
					for (let x in firstResultObj) {
						var theader = '<th class="row-query-results">' + x +'</th>';
						$(".row-header").append(theader);
					}								

					var chartLabel = [];
					var co = [];
 					var co_1 = [];
					var co_2 = [];
					var co_3 = [];
					var co_4 = [];
					var co_5 = [];
					var co_6 = [];
					var co_7 = [];
					
					var mintemp = [];
					var maxtemp = [];
					var pressure = [];
					let count = 1;
					var countCoeffSequence = 0;

					var chartLabelRate = [];
					var ae = [];
					var ef = [];
					var te = [];
					
					for (let result of resultArray) {
						countCoeffSequence = countCoeffSequence + 1;
						jsonString = result + '}'
						jsonString = jsonString.replace(/\n +/g, "");
						resultObj = JSON.parse(jsonString);						
						$.each(resultObj, function(i, row) {
						  if(search_querySelection == 'compthermo'){
							if (i == 'CoefficientValues') {
					        	var coefficients = row.split(',');					        		
					        	$.each(coefficients, function(index, value) {
					        		 co[index] = value;
					        	});	
					        	
					        	co_1.push(co[0]); 
					        	co_2.push(co[1]); 
					        	co_3.push(co[2]); 
					        	co_4.push(co[3]); 
					        	co_5.push(co[4]); 
					        	co_6.push(co[5]); 
					        	co_7.push(co[6]); 

					        } else if (i == 'MinTemp') {					        	
					        	mintemp.push(row);
					        	
					        } else if (i == 'MaxTemp') {
					        	maxtemp.push(row);

					        } else if (i == 'MechanismName') {
					        	if(countCoeffSequence % 2 == 0){
					        		chartLabel.push(formatLabel(row) + ' (HTR)');
					        	} else{
					        		chartLabel.push(formatLabel(row) + ' (LTR)');
					        	}
					        	
					        } else if (i == 'Pressure') {
					        	pressure.push(row);
					        }
						  } else if(search_querySelection == 'comparerate' || search_querySelection == 'comparerateAnyOrder'){
								if (i == 'ActivationEnergy') {					        	
						        	ae.push(row);
						        } else if (i == 'PreExpFactor') {
						        	ef.push(row);
						        } else if (i == 'TempExponent') {
						        	te.push(row);
						        } else if (i == 'MechanismName') {
						        		chartLabelRate.push(formatLabel(row));
						        }							  
						  }
					      });
						
						if (search_querySelection != 'compthermo' && search_querySelection != 'comparerate' && search_querySelection != 'comparerateAnyOrder') {
							queryResultsTable.append(getTableResultRowString(count++, resultObj));
							$("#chartCanvas").hide();
							$("#chartCanvasRateAE").hide();
							$("#chartCanvasRatePEF").hide();
							$("#chartCanvasRateTE").hide();
							$("#tableMechanism").show();

						} else if(search_querySelection == 'compthermo'){ // show chart
							$("#chartCanvas").show();
							$("#chartCanvasRateAE").hide();
							$("#chartCanvasRatePEF").hide();
							$("#chartCanvasRateTE").hide();
							$("#tableMechanism").hide();
							$("canvas#canvas").remove();
							$("div#chartCanvas").append('<canvas id="canvas" class="animated fadeIn" style="width:800px !important; height:500px"></canvas>');
							var config = {
									type: 'line',
									data: {
										labels: chartLabel,
										datasets: [{
											label: 'a1',
											backgroundColor: window.chartColors.red,
											borderColor: window.chartColors.red,
											data: co_1,
											fill: false,
										}, {
											label: 'a2',
											fill: false,
											backgroundColor: window.chartColors.blue,
											borderColor: window.chartColors.blue,
											data: co_2,
										}, {
											label: 'a3',
											fill: false,
											backgroundColor: window.chartColors.orange,
											borderColor: window.chartColors.orange,
											data: co_3,
										}, {
											label: 'a4',
											fill: false,
											backgroundColor: 'rgb(139,69,19)',
											borderColor: 'rgb(139,69,19)',
											data: co_4,
										}, {
											label: 'a5',
											fill: false,
											backgroundColor: 'rgb(255,99,71)',
											borderColor:  'rgb(255,99,71)',
											data: co_5,
										}, {
											label: 'a6',
											fill: false,
											backgroundColor: 'rgb(46,139,87)',
											borderColor: 'rgb(46,139,87)',
											data: co_6,
										}, {
											label: 'a7',
											fill: false,
											backgroundColor: 'rgb(30,144,255)',
											borderColor: 'rgb(30,144,255)',
											data: co_7,
										}, {
											label: 'Minimum Temperature',
											fill: false,
											backgroundColor: window.chartColors.yellow,
											borderColor: window.chartColors.yellow,
											data: mintemp,
										}, {
											label: 'Maximum Temperature',
											fill: false,
											backgroundColor: window.chartColors.green,
											borderColor: window.chartColors.green,
											data: maxtemp,
										}, {
											label: 'Pressure',
											fill: false,
											backgroundColor: 'rgb(221,160,221)',
											borderColor: 'rgb(221,160,221)',
											data: pressure,
											hidden: true,
										}
										]
									},
									options: {
										responsive: true,
										title: {
											display: true,
											text: ['NASA Polynomial Coefficients for the Low-Temperature and High-Temperature Ranges.', 'Here a1, a2, a3, a4, a5, a6, and a7 are the numerical coefficients.', 'LTR means low-temperature range and HTR means high-temperature range.']
										},
										tooltips: {
											mode: 'index',
											intersect: false,
										},
										hover: {
											mode: 'nearest',
											intersect: true
										},
										scales: {
											xAxes: [{
												display: true,
												scaleLabel: {
													display: true,
													labelString: 'Mechanism'
												}
											}],
											yAxes: [{
												display: true,
												scaleLabel: {
													display: true,
													labelString: 'Coefficients and Temperatures'
												}
											}]
										}
									}
								};

								var ctx = document.getElementById('canvas').getContext('2d');
								var myChart = new Chart(ctx, config);
								 
						} else if(search_querySelection == 'comparerate' || search_querySelection == 'comparerateAnyOrder'){ // show chart
							$("#chartCanvasRateAE").show();
							$("#chartCanvasRatePEF").show();
							$("#chartCanvasRateTE").show();
							$("#chartCanvas").hide();
							$("#tableMechanism").hide();
							$("canvas#canvasRateAE").remove();
							$("canvas#canvasRatePEF").remove();
							$("canvas#canvasRateTE").remove();
							$("div#chartCanvasRateAE").append('<canvas id="canvasRateAE" class="animated fadeIn" style="width:800px !important; height:300px"></canvas>');
							$("div#chartCanvasRatePEF").append('<canvas id="canvasRatePEF" class="animated fadeIn" style="width:800px !important; height:300px"></canvas>');
							$("div#chartCanvasRateTE").append('<canvas id="canvasRateTE" class="animated fadeIn" style="width:800px !important; height:300px"></canvas>');
							var configAE = {
									type: 'line',
									data: {
										labels: chartLabelRate,
										datasets: [{
											label: 'Activation Energy',
											backgroundColor: window.chartColors.red,
											borderColor: window.chartColors.red,
											data: ae,
											fill: false,
										}
										]
									},
									options: {
										responsive: true,
										title: {
											display: true,
											text: ['Activation Energy for the Given Reaction across Mechanisms.']
										},
										tooltips: {
											mode: 'index',
											intersect: false,
										},
										hover: {
											mode: 'nearest',
											intersect: true
										},
										scales: {
											xAxes: [{
												display: true,
												scaleLabel: {
													display: true,
													labelString: 'Mechanism'
												}
											}],
											yAxes: [{
												display: true,
												scaleLabel: {
													display: true,
													labelString: 'Activation Energy (J/mol)'
												}
											}]
										}
									}
								};

								var ctxAE = document.getElementById('canvasRateAE').getContext('2d');
								var myChartAE = new Chart(ctxAE, configAE);

								var configPEF = {
										type: 'line',
										data: {
											labels: chartLabelRate,
											datasets: [{
												label: 'Pre-exponential Factor',
												fill: false,
												backgroundColor: window.chartColors.blue,
												borderColor: window.chartColors.blue,
												data: ef,
											}
											]
										},
										options: {
											responsive: true,
											title: {
												display: true,
												text: ['Pre-exponential Factor for the Given Reaction across Mechanisms.']
											},
											tooltips: {
												mode: 'index',
												intersect: false,
											},
											hover: {
												mode: 'nearest',
												intersect: true
											},
											scales: {
												xAxes: [{
													display: true,
													scaleLabel: {
														display: true,
														labelString: 'Mechanism'
													}
												}],
												yAxes: [{
													display: true,
													scaleLabel: {
														display: true,
														labelString: 'Pre-exponential Factor (m3/mol/s)'
													}
												}]
											}
										}
									};

									var ctxPEF = document.getElementById('canvasRatePEF').getContext('2d');
									var myChartPEF = new Chart(ctxPEF, configPEF);
									
									var configTE = {
											type: 'line',
											data: {
												labels: chartLabelRate,
												datasets: [{
													label: 'Temperature Exponent',
													fill: false,
													backgroundColor: window.chartColors.orange,
													borderColor: window.chartColors.orange,
													data: te,
												}
												]
											},
											options: {
												responsive: true,
												title: {
													display: true,
													text: ['Temperature Exponent for the Given Reaction across Mechanisms.']
												},
												tooltips: {
													mode: 'index',
													intersect: false,
												},
												hover: {
													mode: 'nearest',
													intersect: true
												},
												scales: {
													xAxes: [{
														display: true,
														scaleLabel: {
															display: true,
															labelString: 'Mechanism'
														}
													}],
													yAxes: [{
														display: true,
														scaleLabel: {
															display: true,
															labelString: 'Temperature Exponent'
														}
													}]
												}
											}
										};

										var ctxTE = document.getElementById('canvasRateTE').getContext('2d');
										var myChartTE = new Chart(ctxTE, configTE);
						}
						
					}
					$("#num-results").text(`${count-1} results found.`);
				}

				$("#spinner").hide();
			},
			error: (XMLHttpRequest, textStatus, errorThrown) => { 
				alert("INCORRECT SPARQL QUERY!")
	        }
		})
		}
	})
	
});


</script>

</body>
</html>