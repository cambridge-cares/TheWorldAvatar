<%@ page language="java"
         contentType="text/html; charset=US-ASCII"
         pageEncoding="US-ASCII"%>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "https://www.w3.org/TR/html4/loose.dtd">
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=US-ASCII">

        <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js"></script>
        <script src='./dashboard.js' type='text/javascript'></script>
        <link href='./style.css' type='text/css' rel='stylesheet'>

        <title>StatusAgent Dashboard</title>

        <%@ page import="uk.ac.cam.cares.jps.agent.status.TemplateHandler" %>
        <%@ page import="uk.ac.cam.cares.jps.agent.status.define.*" %>
        <%@ page import="uk.ac.cam.cares.jps.agent.status.record.*" %>

        <%@ page import="java.util.Map" %>
        <%@ page import="java.util.Set" %>

    </head>
    <body onload="initialise()">
        <div id="preamble-dashboard">
            <div id="title">
                <h1>StatusAgent - Dashboard</h1>
            </div>
            <div id="introduction">
                <p>
                    This page shows the results of the most recent executions of all tests within the StatusAgent framework.
                    These results are grouped under each test type, and indicate whether the test has yet to be executed, run successfully, or run with a failure.
                </p>
                <p>
                    To see a complete list of all executions for each defined test, or view the logs from individual executions, click on the test box below.
                </p>
            </div>
        </div>
        <div class="content-dashboard">
            <div class="all-tests-container">
                <%
                    // Get the map of all defined tests
                    Map<TestType, Set<TestDefinition>> testDefinitions = (Map<TestType, Set<TestDefinition>>) request.getAttribute("test-definitions");

                    // Get the map of latest test results
                    Map<TestDefinition, TestRecord> testResults = (Map<TestDefinition, TestRecord>) request.getAttribute("test-results");

                    // Get counts of failures and successes per test type
                    Map<TestType, Integer> failureCounts = (Map<TestType, Integer>) request.getAttribute("test-failures");
                    Map<TestType, Integer> successCounts = (Map<TestType, Integer>) request.getAttribute("test-successes");

                    // For each type of test
                    for (Map.Entry<TestType, Set<TestDefinition>> entry : testDefinitions.entrySet()) {

                        out.print("<div class='test-container'>");
                        out.print("<div class='test-container-title center-text'>");
                        out.print("<h3>" + TestType.getFriendlyName(entry.getKey()) + "</h3>");
                        out.print("</div>");

                        if (failureCounts.containsKey(entry.getKey()) && successCounts.containsKey(entry.getKey())) {
                            out.print("<div class='summary-count center-text'>");
                            out.print("<p>Success: " + successCounts.get(entry.getKey()));
                            out.print(",  Failure: " + failureCounts.get(entry.getKey()) + "</p>");
                            out.print("</div>");
                        }

                        // For each individual test
                        for (TestDefinition definition : entry.getValue()) {
                            TestRecord latestResult = testResults.get(definition);
                            out.print(TemplateHandler.getDashboardStub(pageContext.getServletContext(), definition, latestResult));
                        }
                        out.print("</div>");
                    }
                %>
            </div>
            <div class="button-container">
                <div class="run-test-button center-text" onclick="runTests()">
                    <p>Run All Tests Now</p>
                </div>
            </div>
        </div>

    </body>
</html>