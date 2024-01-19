<?xml version="1.0" encoding="UTF-8" ?>
<%@ page contentType="text/html; charset=UTF-8"%>
<%@ taglib prefix="s" uri="/struts-tags"%>
<%@ taglib prefix="sj" uri="/struts-jquery-tags"%>
<%@ taglib prefix="sb" uri="/struts-bootstrap-tags"%>

<!DOCTYPE html>
<html>
<!--  after pressing refresh button it clears content of page. -->
<head>


<link rel="icon"
	href="${pageContext.request.contextPath}/css/static/group/favicon.ico" />

<link rel="stylesheet" type="text/css"
	href="${pageContext.request.contextPath}/css/static/group/CoMoStyle.css">

<link rel="stylesheet" type="text/css"
	href="${pageContext.request.contextPath}/css/static/group/CoMoIncludeStyle.css">

<link rel="stylesheet" type="text/css"
	href="${pageContext.request.contextPath}/css/static/group/CoMoThemesStyle.css">

<title>Computational Modelling Group</title>

</head>

<body class="oneColFixCtr">
	<div id="container">
		<div id="mainContent">

			<div id="border">
				<div id="skiplinks">
					<ul>
						<li><a href="/" accesskey="h">Jump to Home [Accesskey
								'h']</a></li>
						<li><a href="/people/" accesskey="m">Jump to People
								[Accesskey 'm']</a></li>
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

				<div id="top-nav" class="clearfix">
					<ul class="clearfix">
						<li class="az"><a id="az"
							href="http://www.cam.ac.uk/university-a-z">a-z</a></li>
						<li class="search"><a id="search" href="http://search.cam.ac.uk/">search</a></li>
						<li class="home"><a id="home" href="http://como.ceb.cam.ac.uk">home</a></li>
					</ul>
				</div>

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

				<div id="primary-navigation" class="clearfix">
					<ul class="clearfix">
					    
					    <!--<li class="clearfix"><a href='/'>Home</a></li> -->
						<li class="clearfix"><a href='http://como.ceb.cam.ac.uk'>Home</a></li>
						
						<!-- <li class="clearfix"><a href='/people/'>People</a></li>-->
						<li class="clearfix"><a href='http://como.ceb.cam.ac.uk/people/'>People</a></li>	
											
						<!-- <li class="clearfix"><a href='/research/'>Research</a></li>-->
						 <li class="clearfix"><a href='http://como.ceb.cam.ac.uk/research/'>Research</a></li>
						 
						<!--<li class="clearfix"><a class='selected-menu-item' href='/resources/'>Resources</a></li>-->
						<li class="clearfix"><a class='selected-menu-item'>Resources</a></li>
						
						<!-- <li class="clearfix"><a href='/preprints/'>Preprints</a></li> -->
						<li class="clearfix"><a href='http://como.ceb.cam.ac.uk/preprints/'>Preprints</a></li>
						
						<!-- <li class="clearfix"><a href='/publications/'>Publications</a></li>-->
						<li class="clearfix"><a href='http://como.ceb.cam.ac.uk/publications/'>Publications</a></li>
						
						<!-- <li class="clearfix"><a href='/conferences/'>Conferences</a></li>-->
						<li class="clearfix"><a href='http://como.ceb.cam.ac.uk/conferences/'>Conferences</a>
						
						<!--<li class="clearfix"><a href='/seminars/'>Seminars</a></li>-->
						<li class="clearfix"><a href='http://como.ceb.cam.ac.uk/seminars/'>Seminars</a></li>
						
						<!--<li class="clearfix"><a href='/admin/'>Login</a></li>-->
						<li class="clearfix"><a href='http://como.ceb.cam.ac.uk/admin/login/?next=/admin/'>Login</a></li>
					
					</ul>
				</div>



				<div id="leftnav" class="clearfix">
					<ul>
						<!-- <li class="sel last"><a href="/preprints/">Introduction</a></li>-->
						<li class="sel last"><a href="https://como.ceb.cam.ac.uk/resources/">Introduction</a></li>
					</ul>					
					<ul><li class="sel last"><a href="http://como.ceb.cam.ac.uk/resources/software/">Software</a></li></ul>
					<ul><li class="sel last"><a href="https://como.ceb.cam.ac.uk/resources/sootdb/">Soot Database</a></li></ul>					
					<ul><li class="selcsm">MolHub</li></ul>
					<ul><li class="sel last"><a href="http://como.ceb.cam.ac.uk/resources/flpyro/">Flame pyrometry</a></li></ul>
					
				</div>

				<!--<s:url action="upload" var="uAction" />-->
				

				<!-- SUB-SUB-MENU, FEEL FREE TO CHANGE -->
				
				<div class="Applications" id="subsubmenu">
					<ul class="clearfix">
						<!-- <li class="clearfix"><s:a class="selected" href="%{uAction}">Upload Gaussian files</s:a></li>-->
						 <!-- <li class="clearfix"><a class="unselected" href="/molhub/query/">Query Compchem repository</a></li>-->
						 
						 <li class="clearfix"><s:a class="selected" href="upload.jsp">Upload Gaussian files</s:a></li>
						 <li class="clearfix"><s:a class="unselected" href="search.jsp">Query Compchem repository</s:a></li>
					</ul>
				</div>

				<div id="main-content" class="clearfix">
                        
                        <s:actionerror />
						<s:actionmessage />
						
					<!-- PUT CONTENT HERE -->
					<div class="col-md-9">
					
						<s:form action="upload" method="post"
							enctype="multipart/form-data" theme="bootstrap">

							<s:file name="upload" label="Select Gaussian files to upload:"
								multiple="multiple" theme="bootstrap" />

							<s:submit value="Upload" label="Select files" theme="bootstrap" />

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
				<div id="footer-bar"></div>
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

			</div>
		</div>
	</div>
</body>
</html>