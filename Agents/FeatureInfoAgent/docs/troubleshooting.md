# FIA Troubleshooting

This page offers some solutions for common issues, and answers to common questions, that may arise when using the FIA. Users that have encountered, and solved issues, are welcome to add to the table below.


| Issue/Question | Solution/Answer |
| ----------- | ----------- |
| Agent cannot find a class for my instance IRI. | This can happen when your KG (or Ontop mapping) does not contain the required triples to fulfil the class determination queries. You may need to add triples, or update your mapping until these queries return the expected class. |
| Agent still cannot find a class for my instance IRI. | When no `endpoint` parameter is present in the HTTP request, the agent will federate queries across all Blazegraph namespaces. The federation technology used by the agent framework sometimes fails to return valid results; try adding the `endpoint` parameter to your GeoServer table so it becomes present in requests. |