# Downsampling
## Description
This downsampling library is used to downsample Timeseries data. Downsampling reduces the size of data to prevent unnecessary consumption of memory in databases. 
Additionally, it can be used to remove duplicated data that takes up space but produces no value. 
Downsampling is also useful for visualization of trends or statistical analysis to extract insights.

The document outlines the user guide on how to use the downsampling library.

# User Guide
## How to use downsampling library
### Method
To downsample timseries data, call the `downsampleTS` method in this library.  
``` java
downsampleTS (TimeSeries ts, Long resolution, Type type)
```
### Inputs
`ts` is the raw timeseries data to be downsampled.

`resolution` is the time interval in seconds of the timeseries data to be downsampled.

`type` is the type of [downsampling method](#Downsampling-Type). The downsampling type can be specified using the Type 
enum which can assume the following values: 
`Type.MAXIMUM`, `Type.MEDIAN`, `Type.MINIMUM`, `Type.SUM`, `Type.AVERAGE`, `Type.COUNT`, `Type.INSTANTANEOUS`.

### Output
`downsampleTS` returns the downsampled timeseries data.

## Downsampling Type
The Type enum can assume one of the following values:
### 1) Type.MAXIMUM
Retrieves the maximum value of the points within a time resolution to represent the time interval. 
### 2) Type.MEDIAN
Retrieves the median value of the points within a time resolution to represent the time interval.
### 3) Type.MINIMUM
Retrieves the minimum value of the points within a time resolution to represent the time interval.
### 4) Type.SUM 
Retrieves the sum value of all points within a time resolution to represent the time interval.
### 5) Type.AVERAGE
Retrieves the average value of the points within a time resolution to represent the time interval.
### 6) Type.COUNT
Retrieves the total number of all points within a time resolution to represent the time interval.
### 7) Type.INSTANTANEOUS
Retrieves the value of the point closest to the time resolution to represent the time interval.

## Notes
The following should be considered before downsampling of data:
1) Downsampled data should be an accurate representation of the inherent nature/characteristics of the original data.
   - Choose the appropriate downsampling method based on the characteristics of the original data.
2) Data outlier handling
   - This library does not handle outliers and anomalies.
   - Determine the significance of outliers of the dataset and assess whether the removal of outlier is necessary.
3) Ensure there is no null data in each time resolution for the time-series to be downsampled. Data cleaning will be required to post-process of nonsensical data when the input contains null data. 
4) Downsampling library only processes `Double` class, modifications to the code may be required for other data class.
5) Downsampling aggregation follows the left time edge.
   - This downsampling method represents a time interval on the left time edge.
6) Downsampling aggregation follows the left closing.
   - If a data timestamp lands exactly on the edge of a time interval, it falls under the previous/left time interval for downsampling.


