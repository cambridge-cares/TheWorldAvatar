<?xml version="1.0" encoding="UTF-8"?>

<%@ page contentType="text/html; charset=UTF-8"%>
<%@ taglib prefix="s" uri="/struts-tags"%>
<%@ taglib  prefix="c"  uri="http://java.sun.com/jsp/jstl/core"%>

<!DOCTYPE html>
<html>

<head>


<script src="https://cdn.jsdelivr.net/npm/chart.js@2.8.0"></script>


	

<!--after pressing refresh button it clears content of page.-->
<!--<meta http-equiv="refresh" content="300;url=upload.action"/>-->

<meta http-equiv="refresh" content="120; url=http://theworldavatar.com/graph/statisticsAction">

<title>JPS self growing knowledge graph: Statistics</title>

    <link rel="stylesheet" type="text/css" href="pages/css/static/group/CoMoStyle.css"/>
    
    <meta charset="UTF-8">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css">    
    <link rel="stylesheet" type="text/css" href="pages/css/index.css">
    
    <script type="text/javascript" src="jsmol/JSmol.min.js"></script>
    
    <script src="pages/scripts/start.js"></script>     
	   
<style type="text/css">
  
  .noBorder {
    border:none !important;
}
</style>


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
       <h1 id="head1">Knowledge-graph Statistics</h1>
       
	   <span id="readme-button">?</span>
		<span id="readme-text">
		This is the Computational Modelling Group's JPS knowledge graph statistics. It contains information about number of Gaussian calculations in the JPS knowledge graph, number of species, synonims, number of reactions etc. 
		
		<br/>
		<br/>
		The JPS knowledge graph is powered by the Resource Description Framework (RDF) .
        <br/>
        <br/>

<!-- <a href="https://pubs.acs.org/doi/10.1021/acs.jcim.9b00227">link to paper </a>-->
 
<br/>
<br/>
</span>
		
<p id="description">Explore statistics of the JPS knowledge-graph.   </p>

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
    <div class="span-24 paramlist">
        <h2 align="left"></h2>
        
<!--          
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

</tbody >
</table>
-->
<img src = "pages/images/c4e-preprint-236.png" width="500" height="250"/>
<p></p>
<b><a href="https://doi.org/10.1016/j.compchemeng.2020.106813">Linking reaction mechanisms and quantum chemistry: An ontological approach</a></b>
<p></p>
<p></p>
<p></p>
 
<!-- Self growing knowledge graph statistics -->  

<table border="5" style="text-align:center">
 
  <tr>
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td  class="noBorder" width="150"  height="15"/>  
             
      
      
      <td class="noBorder" colspan="6" style="font-size: 20px;">
        <b>Knowledge-graph statistics</b> 
      </td> 
      
      <td class="noBorder" width="150"  height="15"/> 

      <td class="noBorder" width="150"  height="15"/> 
      
  </tr>
   <tr>
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
     
      
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
       
      
            
      <td class="noBorder" width="150"  height="15"/>
       
      
      
      <td class="noBorder" width="150"  height="15"/>
       
      
      
   </tr>
  
   <tr>
      <td class="noBorder" width="150"  height="15"/>
      
      
      <td height="15" width="300" colspan="2" style="font-size: 35px;" style="border: 5px;" bgcolor="lightgray">
      <s:property value="numberOfCalculations"/>
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      <td height="15" width="300" colspan="2" style="font-size: 35px;" style="border: 5px;" bgcolor="lightgray">
       <s:property value="numberOfSpeciesInOntoSpecies"/>
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      <td height="15" width="300" colspan="2" style="font-size: 35px;" style="border: 5px;" bgcolor="lightgray">
       <s:property value="numberOfSynonyms"/>
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
   </tr>
   
      <tr>
      
      <td class="noBorder" width="150"  height="15"/>
      
      <td  class="noBorder"colspan="2" width="300"  height="15">
      <b>Gaussian calculations in OntoCompChem</b> 
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
      <td  class="noBorder" colspan="2" width="300"  height="15">
      <b>Species in OntoSpecies</b>
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
      <td class="noBorder" colspan="2" width="300"  height="15">
       <b>Synonyms in OntoSpecies</b> 
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
   </tr>
   
   <tr>
      <td class="noBorder" width="150"  height="15"/>
      
      <td class="noBorder" width="150"  height="15"/> 
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
       
      
            
      <td class="noBorder" width="150"  height="15"/>
       
      
      
      <td class="noBorder" width="150"  height="15"/>
       
      
      
   </tr>
  
   
   <tr>
      <td class="noBorder" width="150"  height="15"/>
  
      
      
      <td height="15" width="300" colspan="2" style="font-size: 35px;" style="border:5px;" bgcolor="lightgray">
      
      <s:property value="numberOfReactionMechanisms"/>
      </td>
      
      <td class="noBorder">
  
      </td>
      
      <td height="15" width="300" colspan="2" style="font-size: 35px;" style="border:5px;" bgcolor="lightgray">
       <s:property value="numberOfSpeciesInOntoKin"/>
      </td>
      
      <td class="noBorder" width="150"  height="15"/> 
    
      
      
      <td height="15" width="300" colspan="2" style="font-size: 35px;" style="border: 5px;" bgcolor="lightgray">
       <s:property value="numberOfChemicalReactions"/> 
      </td> 
      
      <td class="noBorder" width="150"  height="15"/>    
   
   </tr>
  
   <tr>
      <td class="noBorder" width="150"  height="15"/>
      
      <td class="noBorder" colspan="2" width="300"  height="15">
      <b>Reaction mechanisms in OntoKin</b> 
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
      <td class="noBorder" colspan="2" width="300"  height="15">
      <b>Species in OntoKin</b> 
      </td>      
      
      <td class="noBorder" width="150"  height="15"/>
      
      <td class="noBorder" colspan="2" width="300"  height="15">
      <b>Chemical reactions in OntoKin</b> 
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
   </tr>
   <tr>
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
     
      
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
       
      
            
      <td class="noBorder" width="150"  height="15"/>
       
      
      
      <td class="noBorder" width="150"  height="15"/>
      
</tr>
      
 </table>

<p></p>
<p></p>
<p></p>

<canvas id="myChart"></canvas>

<script>
var arrayList = new Array();
arrayList= new Array('<s:property value="%{labelList}"/>');
var jsList = arrayList.toString();
jsList = jsList.replace("[","");
jsList = jsList.replace("]","");
jsList = jsList.split(",");


var ontoCompChemList = new Array();
ontoCompChemList= new Array('<s:property value="%{ontoCompChemDataSetList}"/>');
var jsOntoCompChemList = ontoCompChemList.toString();
jsOntoCompChemList = jsOntoCompChemList.replace("[","");
jsOntoCompChemList = jsOntoCompChemList.replace("]","");
jsOntoCompChemList = jsOntoCompChemList.split(",");

var ontoKinList = new Array();
ontoKinList= new Array('<s:property value="%{ontoKinDataSetList}"/>');
var jsOntoKinList = ontoKinList.toString();
jsOntoKinList = jsOntoKinList.replace("[","");
jsOntoKinList = jsOntoKinList.replace("]","");
jsOntoKinList = jsOntoKinList.split(",");

var ctx = document.getElementById('myChart').getContext('2d');
var chart = new Chart(ctx, {
    // The type of chart we want to create
    type: 'bar',

 // The data for our dataset
    data: {
       labels: jsList,
        datasets: [{
            label: 'Additional species in OntoCompChem',
            backgroundColor: 'rgb(0, 47, 255)',
            borderColor: 'rgb(0, 47, 255)',
            data: jsOntoCompChemList
        },
        {
            label: 'Additional species in OntoKin',
            backgroundColor: 'rgb(255, 20, 30)',
            borderColor: 'rgb(255, 20, 30)',
            data: jsOntoKinList
        }
        
        ]
    },

    // Configuration options go here
    options: {}
});
</script>

  
<!--  Self growing knowledge graph statistics about chemical reactions and species in chemical reactions. -->


<table border="5" style="text-align:center">
 
  <tr>
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td  class="noBorder" width="150"  height="15"/>  
             
      
      
      <td class="noBorder" colspan="6" style="font-size: 20px;">
        <b>Knowledge-graph: OntoKin species and reactions statistics</b> 
      </td> 
      
      <td class="noBorder" width="150"  height="15"/> 

      <td class="noBorder" width="150"  height="15"/> 
      
  </tr>
   <tr>
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
     
      
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
       
      
            
      <td class="noBorder" width="150"  height="15"/>
       
      
      
      <td class="noBorder" width="150"  height="15"/>
       
      
      
   </tr>
  
   <tr>
      <td class="noBorder" width="150"  height="15"/>
      
      
      <td height="15" width="300" colspan="2" style="font-size: 35px;" style="border: 5px;" bgcolor="lightgray">
       <s:property value="numberOfCabronAndHydrogenSpecies"/> 
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      <td height="15" width="300" colspan="2" style="font-size: 35px;" style="border: 5px;" bgcolor="lightgray">
       <s:property value="numberOfCabronAndHydrogenAndOxygenSpecies"/>  
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      <td height="15" width="300" colspan="2" style="font-size: 35px;" style="border: 5px;" bgcolor="lightgray">
       <s:property value="numberOfNitrogenSpeciesInOntoKin"/>
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
   </tr>
   
      <tr>
      
      <td class="noBorder" width="150"  height="15"/>
      
      <td  class="noBorder"colspan="2" width="300"  height="15">
      <b>Species in OntoKin that contain only carbon and hydrogen atoms.</b> 
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
      <td  class="noBorder" colspan="2" width="300"  height="15">
      <b>Species in OntoKin that contain only carbon and hydrogen and oxygen atoms.</b>
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
      <td class="noBorder" colspan="2" width="300"  height="15">
       <b>Nitrogen species in OntoKin</b> 
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
   </tr>
   
   <tr>
      <td class="noBorder" width="150"  height="15"/>
      
      <td class="noBorder" width="150"  height="15"/> 
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
       
      
            
      <td class="noBorder" width="150"  height="15"/>
       
      
      
      <td class="noBorder" width="150"  height="15"/>
       
      
      
   </tr>
  
   
   <tr>
      <td class="noBorder" width="150"  height="15"/>
          
      <td height="15" width="300" colspan="2" style="font-size: 35px;" style="border:5px;" bgcolor="lightgray">
      <s:property value="numberOfReactionsHydrocarbonSpecies"/> 
      </td>
      
      <td class="noBorder">
  
      </td>
      
      <td height="15" width="300" colspan="2" style="font-size: 35px;" style="border:5px;" bgcolor="lightgray">
     <s:property value="numberOfReactionsThatInvolveOxygenHydrocarbonSpecies"/>
      </td>
      
      <td class="noBorder" width="150"  height="15"/> 
    
      
      
      <td height="15" width="300" colspan="2" style="font-size: 35px;" style="border: 5px;" bgcolor="lightgray">
      <s:property value="numberOfReactionsThatInvolveNitrogenSpecies"/> 
      </td> 
      
      <td class="noBorder" width="150"  height="15"/>    
   
   </tr>
  
   <tr>
      <td class="noBorder" width="150"  height="15"/>
      
      <td class="noBorder" colspan="2" width="300"  height="15">
      <b>Reactions that involve hydrocarbon species</b> 
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
      <td class="noBorder" colspan="2" width="300"  height="15">
      <b>Reactions that involve only oxygenated hydrocarbon species</b> 
      </td>      
      
      <td class="noBorder" width="150"  height="15"/>
      
      <td class="noBorder" colspan="2" width="300"  height="15">
      <b>Reactions that involve nitrogen species</b> 
      </td>
      
      <td class="noBorder" width="150"  height="15"/>
      
   </tr>
   <tr>
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
     
      
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
      
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
        
      
      
      <td class="noBorder" width="150"  height="15"/>
       
      
            
      <td class="noBorder" width="150"  height="15"/>
       
      
      
      <td class="noBorder" width="150"  height="15"/>
      
</tr>   
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