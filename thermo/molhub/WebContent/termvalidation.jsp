<?xml version="1.0" encoding="UTF-8" ?>
<%@ taglib prefix="s" uri="/struts-tags" %>
<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>

<!DOCTYPE html>    
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>Query submission notification</title>
</head>
<body>

<h3>Thank you for your query.</h3>

<s:bean name="uk.ac.ceb.como.molhub.bean.Term" var="termBean"/>

<p>Your query string is: <s:property value="term" /></p>

<p>Your propositional formula is: <s:property value="formula" /> </p>

<p>Your periodic table element is: <s:property value="periodicTableElement" /> </p>

<p>Your propositional formula in conjunctive normal form (CNF) form is: <s:property value="#termBean.name" /> </p>


<p>Is propositional formula satisfiable (true in at least one valuation)? : <s:property value="satisfiable" /></p>


<!-- change with struts2 action.-->
<s:a href="http://localhost:8080/molhub/upload.jsp">Back to upload page</s:a>

</body>
</html>