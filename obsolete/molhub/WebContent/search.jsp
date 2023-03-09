<?xml version="1.0" encoding="UTF-8"?>

<%@ page contentType="text/html; charset=UTF-8"%>
<%@ taglib prefix="s" uri="/struts-tags"%>
<%@ taglib prefix="sj" uri="/struts-jquery-tags"%>
<%@ taglib prefix="sb" uri="/struts-bootstrap-tags"%>
<%@ taglib  prefix="c"  uri="http://java.sun.com/jsp/jstl/core"%>

<!DOCTYPE html>

<html>

<head>

<!--  after pressing refresh button it clears content of page. -->
<!-- <meta http-equiv="refresh" content="300;url=upload.action" /> -->

<link rel="icon" href="${pageContext.request.contextPath}/css/static/group/favicon.ico" />
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/CoMoStyle.css"/>
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/CoMoIncludeStyle.css"/>
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/CoMoThemesStyle.css"/>
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/styles/tool-navigator.css"/>
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/styles/repository.css"/>

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


#tableName td {
text-align: left;
}



</style>

<div class="jumbotron text-center" id="topBanner">


    <a href="http://www.cares.cam.ac.uk/node/454#overlay-context=c4t-research">
        <img  src="images/cam_lang_negativ1%20NEW_0.png">
    </a>
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

		
<p id="description">Explore knowledge graph as an open database for thermochemistry data.   </p>

</div>

<body class="oneColFixCtr">

<!--<div id="container">-->

<!-- <div id="mainContent">-->
<!-- <div id="border">-->
 
 <div id="primary-navigation" class="clearfix">
				
					<ul class="clearfix">
					</ul>
</div>
 
 

<div id="leftnav" class="clearfix">

 <ul><li class="selcsm">Quantum chemistry knowledge graph</li></ul>
    
</div>

<!-- SUB-SUB-MENU, FEEL FREE TO CHANGE -->

<div class="Applications" id="subsubmenu">
    <ul class="clearfix">
       <li class="clearfix"><s:a class="unselected" href="upload.jsp">Upload Gaussian files</s:a></li>
	   <li class="clearfix"><s:a class="selected" href="search.jsp">Query Compchem repository</s:a></li>
    </ul>
</div>

<div id="main-content" class="clearfix">

<!-- PUT CONTENT HERE -->

<div class="col-md-9">
<s:actionerror/>
<s:form action="search" align="left">   
<s:textfield name="term.name" placeholder="(Ti1 and Cl4)" size="80"/>
<s:submit value="Search Knowledge Graph"/>
</s:form>
</div>
<div class="col-md-9">

<s:actionerror/>
<s:actionmessage />

<s:form action="calculation" >
<s:submit value="Run calculation"/>
</s:form>

<!--<s:property value="term"/>-->

<s:if test="%{session.size>0}">
<table style="width:100%;">
<tbody>
<tr>
<td><b>Number of final results:</b></td><td><s:property value="%{session.size}"/></td>
<td><b>Query completed in :</b> </td> <td><s:property value="runningTime"/></td>
</tr>
<tr>

<!-- 
<td>
<s:if test="%{queryResultString.size>0}">
<b>Number of sparql queries generated:</b> <s:property value="%{queryResultString.size}"/> <P/>
<b>Query result for each atom: </b> <P/> 
<s:property value="queryResultString"/>
</s:if>
</td>
-->

</tr>
</tbody>
</table>
</s:if>

</div>

<!-- 
Session result: <s:property value="session"/> 
 -->
 
<div id="tool-container">

      <div id="result-container">
       
            <div id="process-box" class="tool-tab">

<!--A list of search results for given query string-->
                 
<s:iterator value="finalSearchResultSet" var="result">
      
<div id="<s:property  value="uuid"/>" class="box">
    
    <div class="round-top box-header">
    
        <div class="checkbox-wrapper">
            
        </div>
    
    <div class="species-title"> <s:property value="moleculeName"/></div>
    
    </div>
    
<div class="round-bottom box-content">

<img alt="" src="http://<%=request.getHeader("host")%>/data/ontocompchem/<s:property  value="uuid"/>/<s:property  value="uuid"/>.png" class="species-image"/>

<s:url action="moleculeview.action" var="moleculeView">

<!--Define parameter for uuid to be used in query all data in RDF4J repository.-->
<s:param name="uuidName"><s:property  value="uuid"/></s:param>

<s:param name="uuidFileName"><s:property  value="uniqueFileIdentifier"/></s:param>

</s:url>

<div class="species-image" id="middlepanel"></div>
 
        <!-- <div class="species-content">-->
        <div>
            <div>
                <div class="property-name"><s:property value="resultsColumn[0]"/></div>
                <div class="property-value" align="left"><s:property  value="uuid"/></div>
            </div>
            <p/>
            <div>
                <div class="property-name"><s:property value="resultsColumn[1]"/></div>
                <div class="property-value" align="left">
            <s:a href="%{moleculeView}" target="_blank">http://<%=request.getHeader("host")%>/kb/ontocompchem/<s:property  value="uuid"/> </s:a> 
                </div>
            </div>
            <p/>
            <div>
                <div class="property-name"><s:property value="resultsColumn[2]"/></div>
                <div class="property-value" align="left"> <s:property  value="moleculeName"/></div>
            </div>
             
             <p/>
            <div>
                <div class="property-name"><s:property value="resultsColumn[3]"/></div>
                <div class="property-value" align="left"> <s:property  value="basisSet"/></div>
            </div>
            <p/>
            <div>
                <div class="property-name"><s:property value="resultsColumn[4]"/></div>
                <div class="property-value" align="left"> <s:property  value="levelOfTheory"/></div>
            </div>
        </div>    
       <!-- </div>-->
    </div>
</div>

<P/>

</s:iterator>

</div>
</div>
</div>

</div>

<!-- </div>-->
 <!-- </div>-->
<!--</div>-->
</body>
</html>