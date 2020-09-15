<?xml version="1.0" encoding="UTF-8" ?>

<%@ page contentType="text/html; charset=UTF-8"%>
<%@ taglib prefix="s" uri="/struts-tags"%>
<%@ taglib prefix="sj" uri="/struts-jquery-tags"%>
<%@ taglib prefix="sb" uri="/struts-bootstrap-tags"%>

<!DOCTYPE html>
<html>
<!--  after pressing refresh button it clears content of page. -->
<head>
  
<link rel="icon" href="${pageContext.request.contextPath}/css/static/group/favicon.ico" />
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/CoMoStyle.css">
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/CoMoIncludeStyle.css">
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/CoMoThemesStyle.css"> 

<meta charset="UTF-8">
 
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css">
    <link rel="stylesheet" type="text/css" href="css/index.css">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>

    <title>Quantum chemistry knowledge graph</title>
    
	<script src="scripts/start.js"></script>
	
</head>

<style>

TH {

    padding: 5px; align-content:center
    
}




</style>

<div class="jumbotron text-center" id="topBanner">


    <a href="http://www.cares.cam.ac.uk/node/454#overlay-context=c4t-research">
        <img  src="images/cam_lang_negativ1%20NEW_0.png">
    </a>
       <!--<h1 id="head1">Molhub: Тhermochemistry database</h1>-->
       <h1 id="head1">Quantum chemistry knowledge graph: Thermochemistry database</h1>
	   
	   <span id="readme-button">?</span>
		<span id="readme-text">This is the Computational Modelling Group's open knowledge graph for our thermochemistry data. It contains theoretical results that can be navigated and queried both manually through a browser and automatically. 
		
<br/>
<br/>
The knowledge graph is powered by the Resource Description Framework (RDF) and allows sophisticated queries of the data based on the graph pattern relationships between data points. 
These queries are defined using the SPARQL Protocol and RDF Query Language (SPARQL). These queries can be implemented using a variety of different programming languages and make the database extremely flexible.
<br/>
<br/> 
<a href="https://pubs.acs.org/doi/10.1021/acs.jcim.9b00227">link to paper </a>
<br/>
<br/>
</span>

		
<p id="description">Explore knowledge graph as an open database for thermochemistry data.</p>

</div>

<body class="oneColFixCtr">
<!-- <div id="container">-->
<!--<div id="mainContent">-->
<!--<div id="border">-->

<div id="primary-navigation" class="clearfix">
				
					<ul class="clearfix">
					</ul>
</div>
 
<div id="leftnav" class="clearfix">
				 
<ul><li class="selcsm">Quantum chemistry knowledge graph</li></ul>
				
</div>

				<!--<s:url action="upload" var="uAction" />-->
				

				<!-- SUB-SUB-MENU, FEEL FREE TO CHANGE -->
				
				<div class="Applications" id="subsubmenu">
				
					<ul class="clearfix">						 
						 <li class="clearfix"><s:a class="selected" href="upload.jsp">Upload Gaussian files</s:a></li>
						 <li class="clearfix"><s:a class="unselected" href="search.jsp">Query Compchem repository</s:a></li>
					</ul>
					
				</div>

				<div id="main-content" class="clearfix">
                        
                        <s:actionerror />
						<s:actionmessage />
						
					<!-- PUT CONTENT HERE -->
					<div class="col-md-9">
					
					 
						<s:form action="upload" method="post" enctype="multipart/form-data" theme="bootstrap" cssClass="well form-search" align="left">
						
							<s:file name="upload" label="Select Gaussian files to upload:" multiple="multiple" theme="bootstrap"/>
							<s:textfield name="ontoSpeciesIRI" type="text" label="Provide the corresponding OntoSpecies IRI (optional):" theme="bootstrap"/>
							<s:submit value="Upload" label="Select files" theme="bootstrap"/>

						</s:form>
				    
					
					</div>

					<!-- 
					     1. Lists report about all uploaded Gaussian files. 
					     2. DOES NOT check whether Gaussian file is correct or no. 
					     3. Reports generated owl file name. For one uploaded Gaussian file it is possible to report more than one owl file name. 
					     4. Checks whether generated ontology (owl) file is consistent.  
					-->
					
					<table class="borderAll" border="2">
					<tr>
					<s:iterator value="column" var="c">
					<th><s:property /></th>
					</s:iterator>
					</tr>
					
					<s:iterator value="uploadReportList" var="report" status="status">
					<tr>
					<td class="nowrap"><s:property value="uuid" /></td>
					<th class="nowrap"><s:property value="gaussianFileName"/></th>
					<td class="nowrap"><s:property value="owlFileName"/></td>
					<th class="nowrap"><s:property value="consistencyCompchemOntologyFile"/></th>
					<th class="nowrap"><s:property value="comment"/></th>
					</tr>							
					</s:iterator>
					
					</table>
				</div>

			<!--</div>-->
		<!--</div>-->
<!-- </div>-->
</body>
</html>