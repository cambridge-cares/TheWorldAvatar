## Introduction ##

This documentation outlines the queries (provided in the .sql files) used to assess individual exposure to various food retailers. It provides an in-depth look at both the conceptual aspect and the technical implementations of each query discussed below.

## Queries ##

Below are descriptions of the queries used to calculate people's exposure to food retailers:

**Distance Query:**<br/>
The distance_query.sql file evaluates exposure based on proximity. It measures the distance from an individual's GPS location to nearby food retailers, returning them and counting the number within a 100-metre radius.

**Point Buffer Query:**<br/>
The point_buffer_query.sql file uses a buffer zone approach. It creates a 100-metre buffer around an individual's location and checks how many retailers intersect with this buffer, effectively retrieving and counting retailers within proximity.

**Line Buffer Query:**<br/>
The line_buffer_query.sql file also uses a 100-metre buffer but starts by forming a line from a single data point. It then creates a buffer around this line and retrieves and counts food retailers intersecting with the buffer.

*Note:* The Line Buffer Query initially forms a line, which conceptually suggests a path might be considered; however, because it starts from a single point, the covered area remains similar to that of the Point Buffer Query.

**Daily Path Area Query:**<br/>
The daily_path_area_query.sql file retrieves and counts food retailers that intersect with the daily path area, which is formed by a 100-metre buffer around the line(s) created with the set of data points for a day.

## Discussion ##
All three queries—Distance, Point Buffer, and Line Buffer—assess areas within a 100-metre radius around each data point, effectively yielding the same exposure results. The primary difference lies in their execution times due to their varying computational demands. The Distance Query is the quickest, whereas the Point Buffer Query requires the most resources. Testing against a trajectory dataset of 365 data points and a food retailer dataset comprising 932 entries yielded the following execution times, indicating their performance.

|                      | Execution Time (s) |
|----------------------|--------------------|
| Distance Query       |          1.5       |
| Point Buffer Query   |        116.0       |
| Line Buffer Query    |         68.8       |

The Daily Path Area query outperforms all the others, being approximately five times faster than the Distance query, taking only 0.37 seconds.

## Authors ##
Feroz Farazi (msff2@cam.ac.uk), May 2024