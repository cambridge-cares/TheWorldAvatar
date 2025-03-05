# Ouraring agent

## Requirements

Users instantiated in the timeline."ouraRing" table with their corresponding Ouraring API key.

Following triple to be queried from Ontop (USER_ID is replaced with the corresponding KeyCloak user id):

SELECT ?user
WHERE {
?user rdfs:label "USER_ID"
}

## Optional Requirements

Optional environment variable: NAMESPACE
If the NAMESPACE parameter is not "kb", the namespace needs to be created before running the agent

## Routes

1) POST "/"

   Optional parameters:
   1) start_datetime
   2) end_datetime

   If bounds are not given, the agent will set the upperbound to current time and lowerbound to 30 days before current time. This agent will download and instantiate heart rate data for the users in the timeline."ouraRing" table for the specified time bounds.

   Example request:

   ```bash
   curl -X POST http://localhost:3838/ouraring-agent/
   ```

   ```bash
   curl -X POST "http://localhost:3838/ouraring-agent/?start_datetime=2024-11-20T00:00:00Z&end_datetime=2024-11-22T00:00:00Z"
   ```
