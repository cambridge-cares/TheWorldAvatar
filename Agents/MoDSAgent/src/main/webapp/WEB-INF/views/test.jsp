<%--
  Created by IntelliJ IDEA.
  User: Feroz
  Date: 26/07/18
  Time: 16:45
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<html>
<head>
    <title></title>
    <script src="<c:url value='/assets/js/external/jquery-1.10.2.js' />"></script>
    <script src="<c:url value='/assets/js/external/arbor/arbor.js' />"></script>
    <script src="<c:url value='/assets/js/external/arbor/arbor-tween.js' />"></script>
    <script src="<c:url value='/assets/js/external/arbor/graphics.js' />"></script>
    <script src="<c:url value='/assets/js/external/arbor/renderer.js' />"></script>
    <link rel="stylesheet" type="text/css" href="<c:url value='/assets/js/external/arbor/site.css' />"/>
</head>
<body>

<section style="border: gray solid 5px; width: 1200px; margin: 25px auto;">
<canvas id="viewport" width="1200" height ="800" ></canvas>
</section>

<script src="<c:url value='/assets/js/external/arbor/test3.js' />"></script>
</body>
</html>
