<?xml version="1.0" encoding="UTF-8" ?>
<%@ page contentType="text/html; charset=UTF-8" %>
<%@ taglib prefix="s" uri="/struts-tags" %>
<%@ taglib prefix="sj" uri="/struts-jquery-tags" %>
<%@ taglib prefix="sb" uri="/struts-bootstrap-tags" %> 

<!DOCTYPE html>
<html>

<head>
<!--  after pressing refresh button it clears content of page. -->
<!-- <meta http-equiv="refresh" content="300;url=upload.action" /> -->
<link rel="icon" href="${pageContext.request.contextPath}/css/static/group/favicon.ico" />
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/CoMoStyle.css">
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/CoMoIncludeStyle.css">
<link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/CoMoThemesStyle.css">
<title>Computational Modelling Group</title>
  
</head>

<body class="oneColFixCtr">
<div id="container">
<div id="mainContent">

<div id="border">
    <div id="skiplinks">
        <ul>
            <li><a href="/" accesskey="h">Jump to Home [Accesskey 'h']</a></li>
            <li><a href="/people/" accesskey="m">Jump to People [Accesskey 'm']</a></li>
            <li><a href="/research/" accesskey="r">Jump to Research [Accesskey 'r']</a></li>
            <li><a href="/resources/" accesskey="s">Jump to Resources [Accesskey 's']</a></li>
            <li><a href="/preprints/" accesskey="p">Jump to Preprints [Accesskey 'p']</a></li>
            <li><a href="/publications/" accesskey="b">Jump to Publications [Accesskey 'b']</a></li>
            <li><a href="/conferences/" accesskey="c">Jump to Conferences [Accesskey 'c']</a></li>
            <li><a href="/seminars/" accesskey="s">Jump to Seminars [Accesskey 's']</a></li>
            <li><a href="/admin/" accesskey="l">Jump to Login Page [Accesskey 'l']</a></li>
        </ul>
    </div>

    <div id="top-nav" class="clearfix">
        <ul class="clearfix">
            <li class="az"><a id="az" href="http://www.cam.ac.uk/global/az.html">a-z</a></li>
            <li class="search"><a id="search" href="index.php?Page=Search">search</a></li>
            <li class="home"><a id="home" href="/">home</a></li>
        </ul>
    </div>

    <div id="banner" class="default clearfix">
        <a href="/"><img id="banner-logo" src="${pageContext.request.contextPath}/css/static/group/images/CoMo_Head_01.png" alt="Go to the CoMo homepage"/></a>
        <img id="banner-center" src="${pageContext.request.contextPath}/css/static/group/images/CoMo_Head_02.png" alt="center banner image"/>
        <div id="banner-right">
            <div id="right-logo">
                <a href="http://www.cam.ac.uk/"><img src="${pageContext.request.contextPath}/css/static/group/images/CoMo_Head_03.png" alt="University of Cambridge Logo"/></a>
                <a href="http://www.ceb.cam.ac.uk/"><img src="${pageContext.request.contextPath}/css/static/group/images/CoMo_Head_04.png" alt="University of Cambridge Logo"/></a>
            </div>
        </div>
    </div>

<div id="primary-navigation" class="clearfix">
    <ul class="clearfix">
        <li class="clearfix"><a href='/'>Home</a></li>
        <li class="clearfix"><a href='/people/'>People</a></li>
        <li class="clearfix"><a href='/research/'>Research</a></li>
        <li class="clearfix"><a class='selected-menu-item' href='/resources/'>Resources</a></li>
        <li class="clearfix"><a href='/preprints/'>Preprints</a></li>
        <li class="clearfix"><a href='/publications/'>Publications</a></li>
        <li class="clearfix"><a href='/conferences/'>Conferences</a></li>
        <li class="clearfix"><a href='/seminars/'>Seminars</a></li>
        <li class="clearfix"><a href='/admin/'>Login</a></li>
    </ul>
</div>

<div id="leftnav" class="clearfix">
    <ul><li class="sel last"><a href="/preprints/">Introduction</a></li></ul>
    <ul><li class="selcsm">MolHub</li></ul>
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
<s:form action="search">   
<s:textfield name="term.name" placeholder="Search Molhub" size="80"/>
<s:submit value="Molhub Search"/>
</s:form>
</div>
<div class="col-md-4">
<s:actionerror/>
<s:actionmessage />

<s:form action="calculation" >
<s:submit value="Run calculation"/>
</s:form>
<!-- 
<s:a action="calculation">Run calculation</s:a>
 -->
</div>
 
<!-- 
<s:iterator value="queryResultString">
<b><s:property/></b>
</s:iterator>
 -->
<P/>

<div id="tool-container">

       <div id="result-container">
            <div id="process-box" class="tool-tab">
               
<!-- A list of search results for given query string-->
                   
<s:iterator value="finalSearchResultSet" var="result" >                    
<div id="<s:property  value="uuid"/>" class="box">
    <div class="round-top box-header">
        <div class="checkbox-wrapper" >
            
        </div>
            <div class="species-title" style="background-color:lightblue"><b><s:property  value="moleculeName"/></b></div>
        
    </div>
    <div class="round-bottom box-content" style="background-color:lightyellow">
    <!--<img alt="" src="http://como.cheng.cam.ac.uk/molhub/compchem/6498a583-a210-4ac1/data.3d.thumb.png" class="species-image"/>-->

        <div class="species-content">
            <div>
                <div class="property-name"><b><s:property value="resultsColumn[0]"/></b> <s:property  value="uuid"/></div>
                <div class="property-value"></div>
            </div>
            <p/>
            <div>
                <div class="property-name"></div>
                <div class="property-value">
                   <!-- <a href="http://como.cheng.cam.ac.uk/molhub/compchem/6498a583-a210-4ac1/">http://como.cheng.cam.ac.uk/molhub/compchem/6498a583-a210-4ac1/</a> -->
                </div>
            </div>
            <p/>
            <div>
                <div class="property-name"><b><s:property value="resultsColumn[1]"/></b> <s:property  value="moleculeName"/></div>
                <div class="property-value"></div>
            </div>
            <!-- 
            <div>
                <div class="property-name">InChI</div>
                <div class="property-value">InChI=1S/C2H5O4Si/c1-2-6-7(3,4)5/h2-4H,1H2</div>

            </div>
             -->
             <p/>
            <div>
                <div class="property-name"><b><s:property value="resultsColumn[2]"/></b> <s:property  value="basisSet"/></div>
                <div class="property-value"></div>
            </div>
            <p/>
            <div>
                <div class="property-name"><b><s:property value="resultsColumn[3]"/></b> <s:property  value="levelOfTheory"/></div>
                <div class="property-value"></div>
            </div>
        </div>
    </div>
</div>
<P/>
</s:iterator>
</div>
</div>
</div>

</div>
    <div id="footer-bar"></div>
    <div id="footer">
        <ul>
            <li><a class="date">&copy;2018</a>
             <a href="/">Computational Modelling Group</a></li>
            <li><a class="separator"> :: </a></li>
            <li><a href="http://www.ceb.cam.ac.uk/">Department of Chemical Engineering and Biotechnology</a></li>
            <li><a class="separator"> :: </a></li>
            <li><a href="http://www.cam.ac.uk/">University of Cambridge</a></li>
            <li><a class="separator"> :: </a></li>
            <li><a href="http://www.cam.ac.uk/about-this-site/privacy-policy/">Privacy Policy</a></li>
        </ul>
    </div>

</div>
</div>
</div>
</body>
</html>