<?xml version="1.0" encoding="UTF-8"?>

<%@ page contentType="text/html; charset=UTF-8"%>
<%@ taglib prefix="s" uri="/struts-tags"%>
<%@ taglib  prefix="c"  uri="http://java.sun.com/jsp/jstl/core"%>

<!DOCTYPE html>
<html>
<!--    
      -->
<head>

<!--after pressing refresh button it clears content of page.-->
<!--<meta http-equiv="refresh" content="300;url=upload.action"/>-->

<meta http-equiv="refresh" content="120; url=http://theworldavatar.com/jpsgraph/statisticsAction">

<title>JPS self growing knowledge graph: Statistics</title>

    <link rel="stylesheet" type="text/css" href="pages/css/static/group/CoMoStyle.css"/>
    
    <meta charset="UTF-8">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css">    
    <link rel="stylesheet" type="text/css" href="pages/css/index.css">
    
    <script type="text/javascript" src="jsmol/JSmol.min.js"></script>
    
     <script src="pages/scripts/start.js"></script>
   
</head>

<style>

TH {

    padding: 5px; align-content:center
    
}


#tableName td {
text-align: left;
}



</style>

<div class="jumbotron text-center" id="topBanner">

    <a href="http://www.cares.cam.ac.uk/node/454#overlay-context=c4t-research">
    <img  src="pages/images/cam_lang_negativ1%20NEW_0.png">
    </a>
       <h1 id="head1">The JPS self growing knowledge graph: Statistics</h1>
       
	   <span id="readme-button">?</span>
		<span id="readme-text">
		This is the Computational Modelling Group's self growing knowledge graph statistics. It contains information about number of Gaussian calculations in the JPS knowledge graph, number of species, synonims, number of reactions etc. 
		
		<br/>
		<br/>
		The JPS self growing knowledge graph is powered by the Resource Description Framework (RDF) .
        <br/>
        <br/>

<!-- <a href="https://pubs.acs.org/doi/10.1021/acs.jcim.9b00227">link to paper </a>-->
 
<br/>
<br/>
</span>
		
<p id="description">Explore statistics for the JPS self growing knowledge graph.   </p>

</div>
<body class="oneColFixCtr">

<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">

<!-- 
<div id="container">
<div id="mainContent">
<div id="border">
-->


<div id="primary-navigation" class="clearfix">
 
    <ul class="clearfix">    
    
    </ul>

</div>

<div id="leftnav" class="clearfix">
    
<ul><li class="selcsm">Statistics</li></ul>
    
</div>

<!-- SUB-SUB-MENU, FEEL FREE TO CHANGE -->



<div id="main-content" class="clearfix">
 
<!-- <div id="main-content" class="clearfix">-->

<!--<div id="tool-container">-->

<!--

<div class="container">

Here is java script for visualization molecules.

</div>

-->
<!-- 
<h1 align="left"><s:iterator value="moleculePropertyList" var="resultSet"><s:property value="#resultSet.moleculeName"/></s:iterator></h1>
-->

<div style="width: 100%;margin: auto;position:left;">    
    <div style="position: left;"class="tabcontent paddingAll hide" id="mol-jmol-1">
        <div style="position:left;" id="middlepanel"></div>
        <div id="leftpanel"></div>
        <div id="rightpanel"></div>
    </div>
</div>

<div class="span-24 last params">
    <div class="span-12 paramlist">
        <h2 align="left"></h2>
        <table style="width:100%;">
<tbody>         
<tr align="left">
<td><b>Number of Gaussian calculations in OntoCompChem:</b></td> <td><b><s:property value="numberOfCalculations"/></b></td>
</tr>
<tr align="left">
<td><b>Number of species in OntoSpecies:</b></td><td><b><s:property value="numberOfSpeciesInOntoSpecies"/></b></td>
</tr>
<tr align="left">
<td><b>Number of synonyms in OntoSpecies:</b></td><td><b><s:property value="numberOfSynonyms"/></b></td>
</tr>

<tr align="left">
<td><b>Number of reaction mechanisms in OntoKin:</b></td><td><b><s:property value="numberOfReactionMechanisms"/></b></td>
</tr>

<tr align="left">
<td><b>Number of species in OntoKin:</b></td><td><b><s:property value="numberOfSpeciesInOntoKin"/></b></td>
</tr>

<tr align="left">
<td><b>Number of chemical reactions in OntoKin :</b></td><td><b><s:property value="numberOfChemicalReactions"/></b></td>
</tr>

<tr align="left">
<td><b>Number of agents in OntoAgent:</b></td><td><b><s:property value="numberOfAgents"/></b></td>
</tr>

</tbody>
</table>

</div>
</div>
<!--</div>-->

</div>

<!-- PUT CONTENT HERE -->
<!--</div>-->
<!-- 
</div>
</div>
-->

</body>
<!-- 
<body>
<h1>JPS self growing knowledge graph statistics:</h1>

<table style="width:50%;">
<tbody>         
<tr align="left">
<td><b>Number Of Gaussian calculations in OntoCompChem:</b></td> <td><b><s:property value="numberOfCalculations"/></b></td>
<td><b>Number Of species in OntoSpecies:</b></td><td><b><s:property value="numberOfSpeciesInOntoSpecies"/></b></td>
</tr>
</tbody>
</table>
</body>
-->
</html>