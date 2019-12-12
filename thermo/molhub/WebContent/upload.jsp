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
 
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
    <link rel="stylesheet" type="text/css" href="css/index.css">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>

    <title>Molhub: The Computational Modelling Group's thermochemistry database</title>
    
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
       <h1 id="head1">Molhub: The Computational Modelling Group's thermochemistry database</h1>
	   
	   <span id="readme-button">?</span>
		<span id="readme-text">This is the Computational Modelling Group's open database for our thermochemistry data. It contains theoretical results that can be navigated and queried both manually through a browser and automatically. 
		
		<br/>
		<br/>
		The database is powered by the Resource Description Framework (RDF) and allows sophisticated queries of the data based on the graph pattern relationships between datapoints. 
		These queries are defined using the SPARQL Protocol and RDF Query Language (SPARQL). These queries can be implemented using a variety of different programming languages and make the database extremely flexible.
		

<br/>
<br/> 

<br/>
<br/>
</span>

		
<p id="description">Exlpore Molhub as an open database for our thermochemistry data.   </p>

</div>

<body class="oneColFixCtr">
<!-- <div id="container">-->
	
		<!--<div id="mainContent">-->

			<!--<div id="border">-->
			<!-- 
				<div id="skiplinks">
 				
					<ul>
					 
						<li><a href="/" accesskey="h">Jump to Home [Accesskey 'h']</a></li>
						<li><a href="/people/" accesskey="m">Jump to People	[Accesskey 'm']</a></li>
						<li><a href="/research/" accesskey="r">Jump to Research
								[Accesskey 'r']</a></li>
						<li><a href="/resources/" accesskey="s">Jump to Resources
								[Accesskey 's']</a></li>
						<li><a href="/preprints/" accesskey="p">Jump to Preprints
								[Accesskey 'p']</a></li>
						<li><a href="/publications/" accesskey="b">Jump to
								Publications [Accesskey 'b']</a></li>
						<li><a href="/conferences/" accesskey="c">Jump to
								Conferences [Accesskey 'c']</a></li>
						<li><a href="/seminars/" accesskey="s">Jump to Seminars
								[Accesskey 's']</a></li>
						<li><a href="/admin/" accesskey="l">Jump to Login Page
								[Accesskey 'l']</a></li>
					
					</ul>

				</div>
		   -->
<!-- 
				<div id="top-nav" class="clearfix">
 
					<ul class="clearfix">
						<li class="az"><a id="az" href="http://www.cam.ac.uk/university-a-z">a-z</a></li>
						<li class="search"><a id="search" href="http://search.cam.ac.uk/">search</a></li>
						<li class="home"><a id="home" href="http://como.ceb.cam.ac.uk">home</a></li>
					</ul>

				</div>
-->
 <!-- 
				<div id="banner" class="default clearfix">
 
					<a href="/"><img id="banner-logo"
						src="${pageContext.request.contextPath}/css/static/group/images/CoMo_Head_01.png"
						alt="Go to the CoMo homepage" /></a> <img id="banner-center"
						src="${pageContext.request.contextPath}/css/static/group/images/CoMo_Head_02.png"
						alt="center banner image" />

					<div id="banner-right">
						<div id="right-logo">
						
 
							<a href="http://www.cam.ac.uk/"><img
								src="${pageContext.request.contextPath}/css/static/group/images/CoMo_Head_03.png"
								alt="University of Cambridge Logo" /></a> <a
								href="http://www.ceb.cam.ac.uk/"><img
								src="${pageContext.request.contextPath}/css/static/group/images/CoMo_Head_04.png"
								alt="University of Cambridge Logo" /></a>

					        
						</div>
					</div>
				</div>
 -->
 
  
				<div id="primary-navigation" class="clearfix">
				
					<ul class="clearfix">
					    
						<!-- <li class="clearfix"><a href='http://como.ceb.cam.ac.uk'>Home</a></li> -->
						
						
						<!-- <li class="clearfix"><a href='http://como.ceb.cam.ac.uk/people/'>People</a></li> -->	
											
						
						 <!--<li class="clearfix"><a href='http://como.ceb.cam.ac.uk/research/'>Research</a></li>-->
						 
						
						<!-- <li class="clearfix"><a class='selected-menu-item'>Resources</a></li>-->
						
						
						<!-- <li class="clearfix"><a href='http://como.ceb.cam.ac.uk/preprints/'>Preprints</a></li>-->
						
						
						<!--<li class="clearfix"><a href='http://como.ceb.cam.ac.uk/publications/'>Publications</a></li>-->
						
						
						<!--<li class="clearfix"><a href='http://como.ceb.cam.ac.uk/conferences/'>Conferences</a>-->
						
						
						<!-- ><li class="clearfix"><a href='http://como.ceb.cam.ac.uk/seminars/'>Seminars</a></li>-->
						
						
						<!-- <li class="clearfix"><a href='http://como.ceb.cam.ac.uk/admin/login/?next=/admin/'>Login</a></li>-->
					
					</ul>
				</div>
 


				<div id="leftnav" class="clearfix">
				
				 <!--
					<ul><li class="sel last"><a href="https://como.ceb.cam.ac.uk/resources/">Introduction</a></li>
					</ul><ul><li class="sel last"><a href="http://como.ceb.cam.ac.uk/resources/software/">Software</a></li></ul>
					<ul><li class="sel last"><a href="https://como.ceb.cam.ac.uk/resources/sootdb/">Soot Database</a></li></ul>
					<ul><li class="sel last"><a href="http://como.ceb.cam.ac.uk/resources/flpyro/">Flame pyrometry</a></li></ul>
			     -->					
					<ul><li class="selcsm">MolHub</li></ul>
				
					
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

						<s:submit value="Upload" label="Select files" theme="bootstrap"/>

						</s:form>
				    
					</div>

					<!-- 
					     1. Lists report about all uploaded Gaussian files. 
					     2. DOES NOT check whether Gaussian file is correct or no. 
					     3. Checks whether XML file is valid against Compchem schema. 
					     4. Checks whether generated ontology (owl) file is consistent.  
					-->
					
					<table class="borderAll" border="1">
						<tr>
							<s:iterator value="column" var="c">
								<th><s:property /></th>
							</s:iterator>

						</tr>
						<s:iterator value="uploadReportList" var="report" status="status">
							<tr>
								<td class="nowrap"><s:property value="uuid" /></td>
								<td class="nowrap"><s:property value="gaussianFileName" /></td>
								<td class="nowrap"><s:property
										value="validationCompchemFile" /></td>
								<td class="nowrap"><s:property
										value="consistencyCompchemOntologyFile" /></td>
							</tr>
							
						</s:iterator>
					</table>


				</div>
				
				<!-- <div id="footer-bar"></div>-->
				<!-- 
				<div id="footer">
					<ul>
						<li><a class="date">&copy;2018</a> <a href="/">Computational
								Modelling Group</a></li>
						<li><a class="separator"> :: </a></li>
						<li><a href="http://www.ceb.cam.ac.uk/">Department of
								Chemical Engineering and Biotechnology</a></li>
						<li><a class="separator"> :: </a></li>
						<li><a href="http://www.cam.ac.uk/">University of
								Cambridge</a></li>
						<li><a class="separator"> :: </a></li>
						<li><a
							href="http://www.cam.ac.uk/about-this-site/privacy-policy/">Privacy
								Policy</a></li>
					</ul>
				</div>
				 -->

			<!--</div>-->
		<!--</div>-->
<!-- </div>-->
</body>
</html>