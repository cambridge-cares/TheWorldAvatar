<%@ page contentType="text/html; charset=UTF-8" %>
<%@ taglib prefix="s" uri="/struts-tags" %>
<%@ taglib prefix="sj" uri="/struts-jquery-tags" %> 

<!--Clear the browser cache in the page used before-->
<%
response.setHeader("Pragma","no-cache");
response.setHeader("Cache-Control","no-store"); 
response.setHeader("Expires", "0");
response.setDateHeader("Expires", -1);
%>
<!DOCTYPE html>
<html>
<head>
<title>Upload Gaussian file</title>
<link rel="stylesheet" type="text/css" href="css/search/search.css">
</head>
<body>

<h3>Upload Gaussian (g09) files:</h3>

<s:actionerror />
<s:form action="upload" method="post" enctype="multipart/form-data">
    <s:file name="upload" label="Select files" multiple="multiple"/>
    <s:submit value="Upload selected files"/>
</s:form>
<s:actionmessage />  
<h4>Uploaded (g09) files are: </h4>
<s:iterator value="uploadFileName" var="fn">
<s:property value="fn"/><br/>
</s:iterator>

<h3>Run thermodynamic calculations:</h3>

<p><a href="<s:url action="calculation"/>">Thermodynamic calculations</a></p>

<h3>Search molecule properties by using atoms</h3>

<s:form action="termValidationAction">
      
 	  <s:textfield name="term.name" placeholder="Search Molhub"/>
   	  
   	  <s:submit value="Molhub Search" align="center"/>
      
</s:form>

</body>
</html>