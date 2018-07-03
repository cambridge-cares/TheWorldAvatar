<%@ page contentType="text/html; charset=UTF-8" %>
<%@ taglib prefix="s" uri="/struts-tags" %>
<html>
<head>
    <title>Upload Gaussian file</title>
</head>

<body>

<h2>Upload Gaussian (g09) file:</h2>

 <s:actionerror />    
<s:form action="upload" method="post" enctype="multipart/form-data">
    <s:file name="upload"/>
    <s:submit/>
</s:form>

<s:iterator value="uploadFileName" var="fn">
 <h4>Uploaded (g09) file is: </h4><s:property value="fn"/><br/>
</s:iterator>

<h2>Run thermodynamic calculations:</h2>

<p><a href="<s:url action="calculation"/>">Thermodynamic calculations</a></p>

</body>
</html>