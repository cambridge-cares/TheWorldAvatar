<%@ page contentType="text/html; charset=UTF-8" %>
<%@ taglib prefix="s" uri="/struts-tags" %>
<%@ taglib prefix="sj" uri="/struts-jquery-tags" %> 
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
      
 	  <s:textfield name="term.name" placeholder="(cl2 or (h2 and o1))"/>
   	  
   	  <s:submit value="Molhub Search" align="center"/>
      
</s:form>

</body>
</html>