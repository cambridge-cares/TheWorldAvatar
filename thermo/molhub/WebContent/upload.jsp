<%@ page contentType="text/html; charset=UTF-8" %>
<%@ taglib prefix="s" uri="/struts-tags" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
    <title>Upload Gaussian file</title>
</head>

<body>

<h3>Upload Gaussian (g09) file:</h3>

 <s:actionerror />    
<s:form action="upload" method="post" enctype="multipart/form-data">
    <s:file name="upload"/>
    <s:submit/>
</s:form>

<s:iterator value="uploadFileName" var="fn">
 <h4>Uploaded (g09) file is: </h4><s:property value="fn"/><br/>
</s:iterator>

<h3>Run thermodynamic calculations:</h3>

<p><a href="<s:url action="calculation"/>">Thermodynamic calculations</a></p>


<h3>Search molecule properties by using atoms</h3>

<s:form action="termValidationAction">

 	  <s:textfield name="term.name" label="Query string" />
   	  <s:submit/>
   	  
</s:form>	

</body>
</html>