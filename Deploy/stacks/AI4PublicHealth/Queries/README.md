## Introduction ##

This documentation outlines the queries (provided in the .sql files) used to assess individual exposure to various food retailers. It provides an in-depth look at both the conceptual aspect and the technical implementations of each query discussed below.

## Queries ##

Below are descriptions of the queries used to calculate people's exposure to food retailers:

**Distance Query:**<br/>
The `distance_query.sql` file evaluates exposure based on proximity. It measures the distance from an individual's GPS location to nearby food retailers, returning them and counting the number within a 100-metre radius.

**Point Buffer Query:**<br/>
The `point_buffer_query.sql` file uses a buffer zone approach. It creates a 100-metre buffer around an individual's location and checks how many retailers intersect with this buffer, effectively retrieving and counting retailers within proximity.

**Line Buffer Query:**<br/>
The `line_buffer_query.sql` file also uses a 100-metre buffer but starts by forming a line from a single data point. It then creates a buffer around this line and retrieves and counts food retailers intersecting with the buffer.

*Note:* The Line Buffer Query initially forms a line, which conceptually suggests a path might be considered; however, because it starts from a single point, the covered area remains similar to that of the Point Buffer Query.

**Daily Path Area Query:**<br/>
The `daily_path_area_query.sql` file retrieves and counts food retailers that intersect with the daily path area, which is formed by a 100-metre buffer around the line(s) created with the set of data points for a day.

**Daily Greenspace Exposure Query:**<br/>
The `daily_greenspace_exposure_query.sql` file retrieves greenspaces that intersect with the daily path area, which is formed by a 100-metre buffer around the line(s) created with the set of data points for a day.

**Trip Wise Greenspace Exposure Count Query:**<br/>
The `trip_wise_greenspace_exposure_count_query.sql` file counts greenspaces that intersect with the trip path area, which is formed by a 100-metre buffer around the line(s) created with the set of data points for a trip.

**Nearest Entity Query:**<br/>
The `nearest_entity_query.sql` file constructs a 100-metre buffer around the daily trajectory to identify food retailers intersecting with the buffer. Each exposed entity is correlated with the nearest GPS point of the daily path. Finally, it enumerates the number of entities associated with each GPS point.

**Nearest Entity Details Query:**<br/>
The `nearest_entity_details_query.sql` file is an enhanced version of the Nearest Entity Details Query, providing the location, name, and address of each exposed food retailer.

**Integral Exposure Query:**<br/>
The `integral_exposure_query.sql` file uses a buffer zone approach. It creates a 100-meter buffer around an individual's location and checks how many retailers intersect with this buffer, effectively retrieving and counting retailers within proximity. It calculates the epoch by taking the difference between the `UTC TIME` of the next row and the `UTC TIME` of the current row, ordered in ascending sequence. Finally, it multiplies the number of retailers by the epoch to calculate the individual's exposure.

## Discussion ##
All three queries—Distance, Point Buffer, and Line Buffer—assess areas within a 100-metre radius around each data point, effectively yielding the same exposure results. The primary difference lies in their execution times due to their varying computational demands. The Distance Query is the quickest, whereas the Point Buffer Query requires the most resources. Testing against a trajectory dataset of 365 data points and a food retailer dataset comprising 932 entries yielded the following execution times, indicating their performance.

|                      | Execution Time (s) |
|----------------------|--------------------|
| Distance Query       |          1.5       |
| Point Buffer Query   |        116.0       |
| Line Buffer Query    |         68.8       |

The Daily Path Area query outperforms the Distance, Point Buffer and Line Buffer queries, being approximately five times faster than the Distance query, taking only 0.37 seconds.

The execution time of the Nearest Entity Query and Nearest Entity Details Query is comparable with that of the Daily Path Query.

The execution time of the Daily Greenspace Exposure query is reasonable, taking less than 200 ms when performed against a synthetic trajectory dataset with 30,000 points and all greenspaces in the UK.

## Test Data Queries ##
The TestDataQueries folder contains queries against test data to cover and explain different test cases, the processes being applied, and their outcomes.

## Authors ##
Feroz Farazi (msff2@cam.ac.uk), May 2024