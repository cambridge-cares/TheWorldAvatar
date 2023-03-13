# SensorLoggerMobileAppQueryAgent
## Description 
This query agent is designed to work with data instantiated by SensorLoggerMobileAppAgent. 


## To use this agent
Send POST request containing the scope and timeframe to query. 

`ewkt` refers to the Polygon coordinates. When specifying the polygon coordinates, the first point and the last point must be the same coordinate to close the loop. 

`upperBound` refers to the start timestamp in OffsetDatetime 

`lowerBound` refers to the end timestamp in OffsetDatetime

Note: In HTTP POST request. spaces are represented with `%20` while `+` sign are represented by `%2B`.

#### Sample Request

```
curl -X POST "http://localhost:10103/SensorLoggerMobileAppQueryAgent/geo?ewkt=POLYGON((103.773082687304%201.3042903514962063,103.773082687304%201.303407520526278,103.77412319164921%201.303407520526278,103.77412319164921%201.3042903514962063,103.773082687304%201.3042903514962063))&lowerBound=2023-03-07T11:13:42.775012200%2B08:00&upperBound=2023-03-10T11:13:42.775012200%2B08:00"
```
