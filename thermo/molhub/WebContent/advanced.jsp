<!DOCTYPE html>
<%@ taglib prefix="s" uri="/struts-tags" %>
<%@ taglib prefix="sj" uri="/struts-jquery-tags" %>
<%@ taglib prefix="sb" uri="/struts-bootstrap-tags" %>
<html>
<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="keywords" content="struts2, twitter, bootstrap, plugin, showcase" />
    
    <sj:head jqueryui="false"/>
    <sb:head includeScripts="true"/>
    <style type="text/css">
        body {
            padding-top: 60px; /* 60px to make the container go all the way to the bottom of the topbar */
        }
    </style>
</head>
<body>

<div class="container">
    <div class="row">
        <div class="col-md-9">
        
            <s:form theme="bootstrap" cssClass="well form-inline">
                <s:textfield
                        placeholder="Login"
                        name="login"
                        tooltip="Enter your Name here"/>

                <s:password
                        placeholder="Password"
                        name="password"/>

                <s:submit cssClass="btn btn-primary"/>
            </s:form>
                        
        </div>
    </div>

    <footer class="footer">
        <p>Created by <a href="http://como.cheng.cam.ac.uk/" target="_blank">CoMo research group</a></p>
    </footer>

</div>

</html>