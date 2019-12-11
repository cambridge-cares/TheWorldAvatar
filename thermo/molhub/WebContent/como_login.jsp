<!DOCTYPE html>
<%@ taglib prefix="s" uri="/struts-tags" %>
<%@ taglib prefix="sj" uri="/struts-jquery-tags" %>
<%@ taglib prefix="sb" uri="/struts-bootstrap-tags" %>

<html>
<head>

<title>Welcome to Molhub</title>

<meta charset="utf-8"/>
<meta http-equiv="X-UA-Compatible" content="IE=edge"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
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
    
    <div class="row justify-content-center">
        
        <div class="col-md-9">
        
        <h2>Molhub Login Application</h2>
        
				<s:actionerror theme="bootstrap"/>
				
				<s:form action="login.action" method="post" theme="bootstrap" cssClass="well form-inline">
					<s:textfield name="username" placeholder="Login"/> <!-- key="label.username" size="20" align="left"  -->
					<s:password name="password" placeholder="Password"/> <!-- key="label.password" size="20" align="left" -->
					<s:submit method="execute" key="label.login" align="center" cssClass="btn btn-primary"/>
				</s:form>
		
			</div>
			
		</div>
	
	<footer class="footer">
        <p>Created by <a href="http://como.cheng.cam.ac.uk/" target="_blank">CoMo research group</a></p>
    </footer>
	
	</div>
</body>
</html>