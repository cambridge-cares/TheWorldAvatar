# Trip agent

This agent processes time series of points to produce trips and visits.

API route:

1) process_trajectory (POST)
   - Assuming this is spun up as a container within a stack using the given config, the agent accepts requests in the following form

    ```bash
    curl -X POST http://localhost:3838/trip-agent/process_trajectory?iri=http://abc&lowerbound=123&upperbound=123
    ```

    Inputs:
    1) iri
       - This IRI should contain a time series of PostGIS points and should contain the following triple

        ```sparql
        <http://abc> <https://www.theworldavatar.com/kg/ontotimeseries/hasTimeSeries> <time_series>
        ```

    2) upperbound
       - Time filter used to query time series

    3) lowerbound
       - Time filter used to query time series  

    Format for upperbound and lowerbound depends on the instantiated time series table, tested with epoch seconds and java.time.Instant. In principle, it should work for any Java time classes with the "parse" method, e.g. ZonedDateTime.

## Instantiation of trips and visits

Trip detection code is adapted from <https://github.com/TeamINTERACT/trip_detection/tree/master>.

The input should be a time series of points:

Time | Location
-- | --
1 | POINT(1,2)
2 | POINT(3,4)
3 | POINT(5,6)
4 | POINT(7,8)

The raw data must contain at least a visit in order for the trip detection module to work (staying within an area over a certain period).

The agent will check for the existence of trip and visit, if they do not exist, they will be instantiated and share the same time series with the point time series. Existing triples before calculations:

```sparql
<http://abc> <https://www.theworldavatar.com/kg/ontotimeseries/hasTimeSeries> <time_series>
```

New triples showing trip and visit sharing the same time series:

```sparql
PREFIX twa: <https://www.theworldavatar.com/kg/>

<trip_iri> rdf:type twa:Trip; <https://www.theworldavatar.com/kg/ontotimeseries/hasTimeSeries> <time_series>.
<visit_iri> rdf:type twa:Visit; <https://www.theworldavatar.com/kg/ontotimeseries/hasTimeSeries> <time_series>.
```

Indices of trips and visits are added to the existing point time series:

Time | Location | Visit | Trip
-- | -- | -- | --
1 | POINT(1,2) | 1| 0
2 | POINT(3,4) | 1| 0
3 | POINT(5,6) | 0| 1
4 | POINT(7,8) | 0| 1

The subject will be either in a visit (trip = 0) or in a trip (visit = 0).

## Development guide

Use [docker-compose.yml](docker-compose.yml) to build a production image, i.e.

```bash
docker compose build
```

Use [docker-compose-debug.yml](docker-compose-debug.yml) to build a debug version, i.e.

```bash
docker compose -f docker-compose-debug.yml build
```

and use [stack-manager/trip-agent-debug.json](stack-manager/trip-agent-debug.json) to spin up the debug container. To attach using VS code, use the following config in launch.json:

```json
    {
        "name": "Python: Attach using Debugpy",
        "type": "debugpy",
        "request": "attach",
        "connect": {
            "host": "localhost",
            "port": 5678
        },
        "pathMappings": [
            {
                "localRoot": "${workspaceFolder}",
                "remoteRoot": "/app"
            }
        ]
    }
```
