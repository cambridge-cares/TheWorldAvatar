# Downsampling
## Description
This downsampling library can be used to post-process and downsample Timeseries data. The document outlines when to downsample a dataset, areas of consideration and a user guide on how it works. 

## When to downsample a dataset
- To reduce the size of data to prevent memory occupation of databases.
- To remove duplicated data within a short time interval that takes up space but produces no value. Query response time will be longer for larger dataset.

## Areas of consideration
1) Downsampled data should be an accurate representation of the inherent nature/characteristics of the original data. 
   - Choose the appropriate downsampling method based on the characteristics of the original data.
2) Purpose and preservation of information
   - Consider the purpose when downsampling data, downsampling can be used for visualization of trends or statistical analysis to extract insights.
3) Data outlier handling
   - Determine the significance of outliers of your dataset. 
   - Assess whether their removal of outlier is necessary to avoid misrepresentation of results.
4) Downsampling resolution size

## User Guide

## How to use downsampling method
``` java
aggregation (TimeSeries ts, Long resolution, int type)
```
`ts` is the raw timeseries data that will be downsampled.

`resolution` is the time interval in number of seconds that will be downsampled. `resolution` is in Long data type. 

`type` is the type of [downsampling method](#Downsampling-Type). Input the number according to the designated number assigned to each type. 

Note:
1) Downsampling aggregation follows the left time edge.
- This downsampling method represents a time interval on the left time edge.
2) Downsampling aggregation follows the left closing.
- If a data timestamp lands exactly on the edge of a time interval, it falls under the previous/left time interval for downsampling.

## Output
Downsampling library returns a downsampled timeseries class.






## Downsampling Type
### 1) Maximum
Retrieves the maximum value of the points within a time resolution to represent the time interval. 
### 2) Median
Retrieves the median value of the points within a time resolution to represent the time interval.
### 3) Minimum
Retrieves the minimum value of the points within a time resolution to represent the time interval.
### 4) Sum 
Retrieves the sum value of all points within a time resolution to represent the time interval.
### 5) Average
Retrieves the average value of the points within a time resolution to represent the time interval.
### 6) Count
Retrieves the total number of all points within a time resolution to represent the time interval.
### 7) Instantaneous
Retrieves the value of the point closest to the time resolution to represent the time interval.




## Notes 
1) Ensure there is no null data in each time resolution for the time-series to be downsampled. Data cleaning will be required to post-process of nonsensical data when null data is inputted. 
2) Downsampling library only processes `Double` class, modifications to the code may be required for other data class. 