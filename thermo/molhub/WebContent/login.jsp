<!DOCTYPE html>
<%@ page contentType="text/html; charset=UTF-8"%>
<%@ taglib prefix="s" uri="/struts-tags"%>

<html>
<head>
<title>Welcome to Molhub</title>
</head>

<body>

<h2>Molhub - Login Application</h2>

<s:actionerror/>
<s:form action="login.action" method="post">
<tr>
<td>
<div class="inputWithIcon">
<s:textfield name="username" key="label.username" size="20" align="left"/>
</div>
<s:password name="password" key="label.password" size="20" align="left"/>
<s:submit method="execute" key="label.login" align="center"/>

</td>
</tr>
</s:form>

</body>
</html>