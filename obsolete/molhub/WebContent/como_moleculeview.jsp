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


<title>Computational Modelling Group</title>

<script   type="text/javascript" src="<%=request.getContextPath()%>/jsmol/JSmol.min.js"></script>

<script type="text/javascript">  

var s = unescape(document.location.search);
var script = 'set errorCallback "myCallback";'
	+'set animationFPS 4;set antialiasdisplay;set measurementUnits angstroms;set zoomlarge false;'
	+'set echo top left;echo loading XXXX...;refresh;'
	+'load ":XXXX";set echo top center;echo XXXX;'
var xxxx = s.split("_USE=")[0]
if (xxxx.length < 2) {
  xxxx = "ethanol"
} else {
  xxxx = xxxx.substring(1);
  if (xxxx.indexOf("load ") >= 0) {
    script = xxxx
    xxxx = ""
  }
}
if (xxxx)
  script = script.replace(/XXXX/g, xxxx)
  
var Info = {
		width:  700,
		height: 600,		
		disableJ2SLoadMonitor: true, 
		disableInitialConsole: true, 
		script: script,
		use: "HTML5",
		jarPath: "<%=request.getContextPath()%>/jsmol/java",
		j2sPath: "<%=request.getContextPath()%>/jsmol/j2s",
		jarFile: "JmolAppletSigned.jar",
		isSigned: false,
		script: "set zoomlarge false;set antialiasDisplay;load http://<%=request.getHeader("host")%>/data/ontocompchem/<s:property  value="uuid"/>/<s:property value="gaussianFileName"/>",		
		addSelectionOptions: false,
		serverURL: "<%=request.getContextPath()%>/jsmol/php/jsmol.php",
		readyFunction: null,
		console: "jmol_infodiv",
		disableInitialConsole: true,
		defaultModel: null,
		debug: false
	}
	
Jmol.getApplet("appletCheck", Info, true);
var isApplet = (appletCheck._jmolType.indexOf("_Applet") >= 0);
var is2D = appletCheck._is2D;

if (!isApplet && !Info.script) {

	// JSmol or image

	Info.defaultModel = "$tylenol";
	Info.script = "#alt:LOAD :tylenol";

}


$(document).ready(function(){
		
	// This demonstration shows that
	// what is put on the page can depend upon the platform.

	// Note that the use of $(document.ready()) is optional but preferred. 
	// You can do the traditional in-body coding if you want. See also simple2-nojq.htm.
	// But as Gusts Kaksis pointed out, if we are using jQuery for database lookups, we might
	// as well use it for more than that.
  
    // note that we create the applet first, before the controls, because
    // we need window.jmol to be defined for those, and Jmol.getAppletHtml does that.
  
  $("#middlepanel").html(Jmol.getAppletHtml("jmol", Info));

  // alternatively, you can use
  //
  //   jmol = "jmol"
  //
  // and then create the buttons before the applet itself. 
  // Just make sure if you do that to use the name of the applet you are
  // actually going to be using. So, perhaps:
  //
  //   jmolApplet0 = "jmolApplet0"
  //

	var use = (Info.use != "JAVA" ? Info.use : Info.isSigned ? "SIGNED" : "JAVA"); 

		$("#leftpanel").html(		
		  "<br>Spin: " + Jmol.jmolRadioGroup(jmol, [["spin off", "off", true],["spin on", "on"]])
		);

  // right panel
  
	Jmol.setButtonCss(null, "style='width:160px'");	
	$("#rightpanel").html(
		Jmol.jmolButton(jmol,"write PNGJ jsmol.png","Save PNG")		
	);
})

</script>

<script src="scripts/start.js"></script>

</head>

<body class="oneColFixCtr">

<div id="container">
<div id="mainContent">

<div id="border">
    <div id="skiplinks">
        <ul>
            <!--<li><a href="/" accesskey="h">Jump to Home [Accesskey 'h']</a></li>-->
            <li><a href="http://como.ceb.cam.ac.uk" accesskey="h">Jump to Home [Accesskey 'h']</a></li>
            
            <!--<li><a href="/people/" accesskey="m">Jump to People [Accesskey 'm']</a></li>-->
            <li><a href="http://como.ceb.cam.ac.uk/people/" accesskey="m">Jump to People [Accesskey 'm']</a></li>
            
            <!--<li><a href="/research/" accesskey="r">Jump to Research [Accesskey 'r']</a></li>-->
            <li><a href="http://como.ceb.cam.ac.uk/research/" accesskey="r">Jump to Research [Accesskey 'r']</a></li>
            
            <!--<li><a href="/resources/" accesskey="s">Jump to Resources [Accesskey 's']</a></li>-->            
            <li><a accesskey="s">Jump to Resources [Accesskey 's']</a></li>
            
            <!--<li><a href="/preprints/" accesskey="p">Jump to Preprints [Accesskey 'p']</a></li>-->
            <li><a href="http://como.ceb.cam.ac.uk/preprints/" accesskey="p">Jump to Preprints [Accesskey 'p']</a></li>
            
            <!--<li><a href="/publications/" accesskey="b">Jump to Publications [Accesskey 'b']</a></li>-->
            <li><a href="http://como.ceb.cam.ac.uk/publications/" accesskey="b">Jump to Publications [Accesskey 'b']</a></li>
            
            <!--<li><a href="/conferences/" accesskey="c">Jump to Conferences [Accesskey 'c']</a></li>-->
            <li><a href="http://como.ceb.cam.ac.uk/conferences/" accesskey="c">Jump to Conferences [Accesskey 'c']</a></li>
            
            <!--<li><a href="/seminars/" accesskey="s">Jump to Seminars [Accesskey 's']</a></li>-->
            <li><a href="http://como.ceb.cam.ac.uk/seminars/" accesskey="s">Jump to Seminars [Accesskey 's']</a></li>            
            
            <!--<li><a href="/admin/" accesskey="l">Jump to Login Page [Accesskey 'l']</a></li>-->
            <li><a href="http://como.ceb.cam.ac.uk/admin/login/?next=/admin/" accesskey="l">Jump to Login Page [Accesskey 'l']</a></li>
        </ul>
    </div>

    <div id="top-nav" class="clearfix">
        <ul class="clearfix">
            <li class="az"><a id="az" href="http://www.cam.ac.uk/university-a-z">a-z</a></li>
            <li class="search"><a id="search" href="http://search.cam.ac.uk/">search</a></li>
            <li class="home"><a id="home" href="http://como.ceb.cam.ac.uk/">home</a></li>
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
        <!--<li class="clearfix"><a href='/'>Home</a></li>-->
        <li class="clearfix"><a href='http://como.ceb.cam.ac.uk'>Home</a></li>
        
        <!--<li class="clearfix"><a href='/people/'>People</a></li>-->
        <li class="clearfix"><a href='http://como.ceb.cam.ac.uk/people/'>People</a></li>
        
        <!--<li class="clearfix"><a href='/research/'>Research</a></li>-->
        <li class="clearfix"><a href='http://como.ceb.cam.ac.uk/research/'>Research</a></li>
        
        <!--<li class="clearfix"><a class='selected-menu-item' href='/resources/'>Resources</a></li>-->
        <li class="clearfix"><a class='selected-menu-item'>Resources</a></li>
        
        <!--<li class="clearfix"><a href='/preprints/'>Preprints</a></li>-->
        <li class="clearfix"><a href='http://como.ceb.cam.ac.uk/preprints/'>Preprints</a></li>
        
        <!--<li class="clearfix"><a href='/publications/'>Publications</a></li>-->
        <li class="clearfix"><a href='http://como.ceb.cam.ac.uk/publications/'>Publications</a></li>
        
        <!--<li class="clearfix"><a href='/conferences/'>Conferences</a></li>-->
        <li class="clearfix"><a href='http://como.ceb.cam.ac.uk/conferences/'>Conferences</a></li>
        
        <!--<li class="clearfix"><a href='/seminars/'>Seminars</a></li>-->
        <li class="clearfix"><a href='http://como.ceb.cam.ac.uk/seminars/'>Seminars</a></li>
        
        <!--<li class="clearfix"><a href='/admin/'>Login</a></li>-->
        <li class="clearfix"><a href='http://como.ceb.cam.ac.uk/admin/login/?next=/admin/'>Login</a></li>
        
    
    </ul>
</div>

<div id="leftnav" class="clearfix">
    <!--<ul><li class="sel last"><a href="/preprints/">Introduction</a></li></ul>-->
    <!-- 
    <ul><li class="sel last"><a href="http://como.cheng.cam.ac.uk/index.php?Page=Resources">Introduction</a></li></ul>
    <ul><li class="selcsm">MolHub</li></ul>
     -->
     
    <ul><li class="sel last"><a href="http://como.ceb.cam.ac.uk/resources/">Introduction</a></li></ul>
    <ul><li class="sel last"><a href="http://como.ceb.cam.ac.uk/resources/software/">Software</a></li></ul>
    <ul><li class="sel last"><a href="https://como.ceb.cam.ac.uk/resources/sootdb/">Soot Database</a></li></ul>       
    <ul><li class="selcsm">MolHub</li></ul>
    <ul><li class="sel last"><a href="http://como.ceb.cam.ac.uk/resources/flpyro/">Flame pyrometry</a></li></ul>
    
</div>

<!-- SUB-SUB-MENU, FEEL FREE TO CHANGE -->
 
<div class="Applications" id="subsubmenu">
    <ul class="clearfix">
       <li class="clearfix"><s:a class="unselected" href="upload.jsp">Upload Gaussian files</s:a></li>
	   <li class="clearfix"><s:a class="unselected" href="search.jsp">Query Compchem repository</s:a></li>
    </ul>
</div>

 
<div id="main-content" class="clearfix">

<div id="tool-container">
<div class="container">
</div><h1><s:iterator value="moleculePropertyList" var="resultSet"><s:property value="#resultSet.moleculeName"/></s:iterator></h1>

<div style="width: 100%;margin: auto;position:left;">    
    <div style="position: left;"class="tabcontent paddingAll hide" id="mol-jmol-1">
        <div style="position:left;" id="middlepanel"></div>
        <div id="leftpanel"></div>
        <div id="rightpanel"></div>
    </div>
</div>
<div class="span-24 last params">
    <div class="span-12 paramlist">
        <h2>Overview<!--{inchi}--></h2>
        <table style="width:80%;">
            <tbody>
                <tr>
                    <td><b>Empirical Formula</b></td>
                    <td><s:iterator value="moleculePropertyList" var="resultSet"><s:property value="#resultSet.moleculeName"/></s:iterator></td>
                </tr>
                <tr>
                    <td><b>Canonical SMILES</b></td>
                    <td>N/A</td>
                </tr>
                <tr>
                    <td><b>Isomeric SMILES</b></td>
                    <td>N/A</td>
                </tr>
                <tr>
                    <td><b>InChI</b></td>
                    <td>N/A</td>
                </tr>
                <tr>
                    <td><b>InChI Key</b></td>
                    <td>N/A</td>
                </tr>
            </tbody>
        </table>
        <h2>Parameters</h2>
        <table style="width:80%;">
            <tbody>
                <tr>
                    <td><b>Basis Set:</b></td>
                    <td><s:iterator value="moleculePropertyList" var="resultSet"> <s:property value="#resultSet.basisSet"/></s:iterator></td>
                </tr>
                <tr>
                    <td><b>Level of Theory:</b></td>
                    <td><s:iterator value="moleculePropertyList" var="resultSet"> <s:property value="#resultSet.levelOfTheory"/></s:iterator></td>
                </tr>
                <tr>
                    <td><b>Geometry Type:</b></td>
                    <td><s:iterator value="moleculePropertyList" var="resultSet"><s:property value="#resultSet.geometryType"/></s:iterator></td>
                </tr>
                <tr>
                    <td><b>Job Type</b></td>
                    <td>N/A</td>
                </tr>

            </tbody>
        </table>
        <h2>Properties</h2>
        <table style="width:100%;">
            <tbody>

                <tr>
                    <s:if test="%{frequencyList.size>0}">
                    <td>
                    <b>Frequencies</b><P/>
                     <s:iterator value="frequencyList" var="result">
                     <ul>
                     <li><b>Frequency Size:</b> <s:property value="#result.frequenciesSize"/></li>
                     <li><b>Frequency Value:</b> <s:property value="#result.frequenciesValue"/></li>
                     <li><b>Frequency Unit:</b> <a href="<s:property value="#result.frequenciesUnit"/>"><s:property value="#result.frequenciesUnit"/></a></li>
                     </ul>
                     </s:iterator>       
                    </td> 
                    </s:if>                   
                    <td></td>
                    <td></td>
                    <td></td>
                    
                </tr>
                <tr>
                    <td><b>Enthalpy of Formation</b></td>
                    <td>N/A</td>
                </tr>
                <s:if test="rotationalSymmetryNumber!=''">
                <tr>
                    <td><b>Rotational Symmetry Number:</b></td>
                    <td><s:property value="rotationalSymmetryNumber"/></td>
                </tr>
                </s:if>
                
                <s:if test="spinMultiplicityValue!=''">
                <tr>
                    <td><b>Spin Multiplicity:</b></td>
                    <td><s:property value="spinMultiplicityValue"/></td>
                </tr>
                </s:if>
                
                <s:if test="%{formalChargeList.size>0}">
                <tr>
                    <td>
                    <b>Formal Charge:</b>
                    <s:iterator value="formalChargeList" var="resultFormalCharge">
                    <ul>
                    <li><b>Formal Charge Value:</b> <s:property value="#resultFormalCharge.formalChargeValue"/></li>
                    <li><b>Formal Charge Unit:</b> <a href="<s:property value="#resultFormalCharge.formalChargeUnit"/>"><s:property value="#resultFormalCharge.formalChargeUnit"/></a></li>
                    </ul>
                    </s:iterator>
                    </td>
                    <td></td>
                    <td></td>
                </tr>
                </s:if>
                
                
                <tr>
                    <td>
                    <s:if test="%{atomicMassList.size>0}">
                    <b>Atomic Mass</b><P/>
                     <s:iterator value="atomicMassList" var="resultAtomicMass">
                     <ul>
                     <li><b>Atom Name:</b> <s:property value="#resultAtomicMass.atomName"/></li>
                     <li><b>Atomic Mass Value:</b> <s:property value="#resultAtomicMass.atomicMassValue"/></li>
                     <li><b>Atomic Mass Unit:</b><a href="<s:property value="#resultAtomicMass.atomicMassUnit"/>"><s:property value="#resultAtomicMass.atomicMassUnit"/></a></li>
                     </ul>
                     </s:iterator>
                     </s:if>                    
                    </td>                    
                    <td></td>
                    <td></td>
                    <td></td>
                </tr>
                
                <tr>
                    <s:if test="%{rotationalConstantList.size>0}">
                    <td>
                    <b>Rotational Constant</b><P/>
                     <s:iterator value="rotationalConstantList" var="resultRotationalConstant">
                     <ul>
                     <li><b>Rotational Constant Size:</b> <s:property value="#resultRotationalConstant.rotationalConstantsSize"/></li>
                     <li><b>Rotational Constant Value:</b> <s:property value="#resultRotationalConstant.rotationalConstantsValue"/></li>
                     <li><b>Rotational Constant Unit:</b> <a href="<s:property value="#resultRotationalConstant.rotationalConstantsUnit"/>"><s:property value="#resultRotationalConstant.rotationalConstantsUnit"/></a></li>
                     </ul>
                     </s:iterator>       
                    </td> 
                    </s:if>                   
                    <td></td>
                    <td></td>
                    <td></td>
                    
                </tr>
                
                
            </tbody>
        </table>
        
        <h2>Other Data</h2>
        <p>
        This page is a human readable frontend to the molecule database.
            The following links provide access to other data formats.
            Depending on your browser these may not be rendered properly.

        <ul>
            <li><a href="http://<%=request.getHeader("host")%>/data/ontocompchem/<s:property  value="uuid"/>/<s:property value="gaussianFileName"/>">Gaussian(G09)</a></li>
  <li><a href="http://<%=request.getHeader("host")%>/data/ontocompchem/<s:property  value="uuid"/>/<s:property value="xmlFileName"/>">XML</a></li>
  <li><a href="http://<%=request.getHeader("host")%>/kb/ontocompchem/<s:property  value="uuid"/>/<s:property value="owlFileName"/>">OWL</a></li>
  <s:if test="%{nasaFileName!=null}">
  <li><a href="http://<%=request.getHeader("host")%>/data/ontocompchem/<s:property  value="uuid"/>/<s:property value="nasaFileName"/>">NASA</a></li>
  </s:if>
        </ul>
        </p>

    </div>
</div>
</div>


</div>

<!-- PUT CONTENT HERE -->
<!-- <div id="middlepanel"></div><div id="leftpanel"></div><div id="rightpanel"></div> -->

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
</body>
</html>