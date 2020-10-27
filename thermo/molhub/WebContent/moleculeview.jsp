<?xml version="1.0" encoding="UTF-8"?>

<%@ page contentType="text/html; charset=UTF-8"%>
<%@ taglib prefix="s" uri="/struts-tags"%>
<%@ taglib prefix="sj" uri="/struts-jquery-tags"%>
<%@ taglib prefix="sb" uri="/struts-bootstrap-tags"%>
<%@ taglib  prefix="c"  uri="http://java.sun.com/jsp/jstl/core"%>

<!DOCTYPE html>
<html>
<!--    
      -->
<head>

<!--after pressing refresh button it clears content of page.-->
<!--<meta http-equiv="refresh" content="300;url=upload.action"/>-->

<title>Quantum chemistry knowledge graph: Thermochemistry database</title>

    <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/css/static/group/CoMoStyle.css"/>
    
    <meta charset="UTF-8">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css">    
    <link rel="stylesheet" type="text/css" href="css/index.css">
    
    <script type="text/javascript" src="jsmol/JSmol.min.js"></script>

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
		script: "set zoomlarge false;set antialiasDisplay;load http://<%=request.getHeader("host")%>/data/ontocompchem/<s:property value="uuid"/>/<s:property value="gaussianFileName"/>",		
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
		<span id="readme-text">
		This is the Computational Modelling Group's open knowledge graph for our thermochemistry data. It contains theoretical results that can be navigated and queried both manually through a browser and automatically. 
		
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

<!--<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">--> 

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
    
<ul><li class="selcsm">Quantum chemistry knowledge graph</li></ul>
    
</div>

<!-- SUB-SUB-MENU, FEEL FREE TO CHANGE -->
 
<div class="Applications" id="subsubmenu">
    <ul class="clearfix">
       <li class="clearfix"><s:a class="unselected" href="upload.jsp">Upload Gaussian files</s:a></li>
	   <li class="clearfix"><s:a class="unselected" href="search.jsp">Query Compchem repository</s:a></li>
    </ul>
</div>

<div id="main-content" class="clearfix">

<h1 align="left"><s:iterator value="moleculePropertyList" var="resultSet"><s:property value="#resultSet.moleculeName"/></s:iterator></h1>

<div style="width: 100%;margin: auto;position:left;">    
    <div style="position: left;"class="tabcontent paddingAll hide" id="mol-jmol-1">
        <div style="position:left;" id="middlepanel"></div>
        <div id="leftpanel"></div>
        <div id="rightpanel"></div>
    </div>
</div>

<div class="span-24 last params">
    <div class="span-12 paramlist">
        <h2 align="left">Overview<!--{inchi}--></h2>
        <table style="width:80%;">
            <tbody>
                <tr align="left">
                    <td><b>Empirical Formula</b></td>
                    <td><s:iterator value="moleculePropertyList" var="resultSet"><s:property value="#resultSet.moleculeName"/></s:iterator></td>
                </tr>
                <tr align="left">
                    <td><b>Canonical SMILES</b></td>
                    <td>N/A</td>
                </tr>
                <tr align="left">
                    <td><b>Isomeric SMILES</b></td>
                    <td>N/A</td>
                </tr>
                <tr align="left">
                    <td><b>InChI</b></td>
                    <td>N/A</td>
                </tr>
                <tr align="left">
                    <td><b>InChI Key</b></td>
                    <td>N/A</td>
                </tr>
            </tbody>
        </table>
        <h2 align="left">Parameters</h2>
        <table style="width:80%;">
            <tbody>
                <tr align="left">
                    <td><b>Basis Set:</b></td>
                    <td><s:iterator value="moleculePropertyList" var="resultSet"> <s:property value="#resultSet.basisSet"/></s:iterator></td>
                </tr>
                <tr align="left">
                    <td><b>Level of Theory:</b></td>
                    <td><s:iterator value="moleculePropertyList" var="resultSet"> <s:property value="#resultSet.levelOfTheory"/></s:iterator></td>
                </tr>
                <tr align="left">
                    <td><b>Geometry Type:</b></td>
                    <td><s:iterator value="moleculePropertyList" var="resultSet"><s:property value="#resultSet.geometryType"/></s:iterator></td>
                </tr>
                <tr align="left">
                    <td><b>Job Type</b></td>
                    <td>N/A</td>
                </tr>

            </tbody>
        </table>
        <h2 align="left">Properties</h2>
        <table style="width:100%;">
            <tbody>

                <tr>
                    <s:if test="%{frequencyList.size>0}">
                    <td align="left">
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
                <tr align="left">
                    <td><b>Enthalpy of Formation</b></td>
                    <td>N/A</td>
                </tr>
                <s:if test="rotationalSymmetryNumber!=''">
                <tr align="left">
                    <td><b>Rotational Symmetry Number:</b></td>
                    <td><s:property value="rotationalSymmetryNumber"/></td>
                </tr>
                </s:if>
                
                <s:if test="spinMultiplicityValue!=''">
                
                <tr align="left">
                    <td><b>Spin Multiplicity:</b></td>
                    <td><s:property value="spinMultiplicityValue"/></td>
                </tr>
                </s:if>
                
                <s:if test="%{formalChargeList.size>0}">
                <tr align="left">
                    <td align="left">
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
                
                
                <tr align="left">
                    <td align="left">
                    <!--<s:if test="%{atomicMassList.size>0}">-->
                    <b>Atomic Mass</b><P/>
                     <s:iterator value="atomicMassList" var="resultAtomicMass">
                     <ul>
                     <li><b>Atom Name:</b> <s:property value="#resultAtomicMass.atomName"/></li>
                     <li><b>Atomic Mass Value:</b> <s:property value="#resultAtomicMass.atomicMassValue"/></li>                     
                     <li><b>Atomic Mass Unit:</b><a href="<s:property value="#resultAtomicMass.atomicMassUnit"/>"><s:property value="#resultAtomicMass.atomicMassUnit"/></a></li>              
                     </ul>
                     </s:iterator>
                     <!--</s:if>-->
                    </td>                    
                    <td></td>
                    <td></td>
                    <td></td>
                </tr>
                
                <tr align="left">
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
                
                
                <tr align="left">
                    <td align="left">
                    <s:if test="%{scfElectronicEnergyList.size>0}">
                    <b>SCF Electronic Energy</b><P/>
                     <s:iterator value="scfElectronicEnergyList" var="resultScfElectronicEnergy">
                     <ul>
                     <li><b>SCF Electronic Energy Value:</b> <s:property value="#resultScfElectronicEnergy.electronicEnergyValue"/></li>
                     <li><b>SCF Electronic Energy Unit:</b><a href="<s:property value="#resultScfElectronicEnergy.electronicEnergyUnit"/>"><s:property value="#resultScfElectronicEnergy.electronicEnergyUnit"/></a></li>
                     </ul>
                     </s:iterator>
                     </s:if>                    
                    </td>                    
                    <td></td>
                    <td></td>
                    <td></td>
                </tr>
                
                
                
                <tr align="left">
                    <td align="left">
                    <s:if test="%{zeroPointElectronicEnergyList.size>0}">
                    <b>Zero-point Electronic Energy</b><P/>
                     <s:iterator value="zeroPointElectronicEnergyList" var="resultZeroPointElectronicEnergy">
                     <ul>
                     <li><b>Zero-point Electronic Energy Value:</b> <s:property value="#resultZeroPointElectronicEnergy.electronicEnergyValue"/></li>
                     <li><b>Zero-point Electronic Energy Unit:</b><a href="<s:property value="#resultZeroPointElectronicEnergy.electronicEnergyUnit"/>"><s:property value="#resultZeroPointElectronicEnergy.electronicEnergyUnit"/></a></li>
                     </ul>
                     </s:iterator>
                     </s:if>                    
                    </td>                    
                    <td></td>
                    <td></td>
                    <td></td>
                </tr>
                
                 <tr align="left">
                    <td align="left">
                    <s:if test="%{electronicAndZeroPointEnergyList.size>0}">
                    <b>Electronic and ZPE Energy</b><P/>
                     <s:iterator value="electronicAndZeroPointEnergyList" var="resultElectronicAndZeroPointEnergy">
                     <ul>
                     <li><b>Zero-point Electronic Energy Value:</b> <s:property value="#resultElectronicAndZeroPointEnergy.electronicEnergyValue"/></li>
                     <li><b>Zero-point Electronic Energy Unit:</b><a href="<s:property value="#resultElectronicAndZeroPointEnergy.electronicEnergyUnit"/>"><s:property value="#resultElectronicAndZeroPointEnergy.electronicEnergyUnit"/></a></li>
                     </ul>
                     </s:iterator>
                     </s:if>                    
                    </td>                    
                    <td></td>
                    <td></td>
                    <td></td>
                </tr>
                
                
                
                
            </tbody>
        </table>
        
        <h2 align="left">Other Data</h2>
        <p align="left">This page is a human readable frontend to the molecule database. The following links provide access to other data formats. Depending on your browser these may not be rendered properly.

<ul>
  <li><a href="http://<%=request.getHeader("host")%>/data/ontocompchem/<s:property value="uuid"/>/<s:property value="gaussianFileName"/>">Gaussian(LOG file)</a></li>
  <!--<li><a href="http://<%=request.getHeader("host")%>/data/ontocompchem/<s:property value="uuid"/>/<s:property value=" gaussianJsonFileName"/>">Gaussian(JSON file)</a></li>-->
  <li><a href="http://<%=request.getHeader("host")%>/kb/ontocompchem/<s:property value="uuid"/>/<s:property value="owlFileName"/>">OWL</a></li>
  <s:if test="%{nasaFileName!=null}">
  <li><a href="http://<%=request.getHeader("host")%>/data/ontocompchem/<s:property value="uuid"/>/<s:property value="nasaFileName"/>">NASA</a></li>
  </s:if>
</ul>
</p>
</div>
</div>

</div>

</body>
</html>