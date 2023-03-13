# Downsampling
## Description
This downsampling library is used to post-process and downsample Timeseries data. Downsampling reduces the size of data to prevent unnecessary consumption of memory in databases. Additionally, it can be used to remove duplicated data that takes up space but produces no value. Downsampling is also useful for visualization of trends or statistical analysis to extract insights.

The document outlines the user guide on how to use the downsampling library.

## User Guide
## How to use downsampling library
### Method
To downsample timseries data, call the `aggregation` method in this library.  
``` java
aggregation (TimeSeries ts, Long resolution, int type)
```
### Inputs
`ts` is the raw timeseries data that to be downsampled.

`resolution` is the time interval in seconds of the timeseries data that will be downsampled.

`type` is the type of [downsampling method](#Downsampling-Type). Input the number according to the designated number assigned to each type. 

### Output
Downsampling library returns a downsampled timeseries data.

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
### Before using downsampling library
The following should be considered before downsampling of data:
1) Downsampled data should be an accurate representation of the inherent nature/characteristics of the original data.
   - Choose the appropriate downsampling method based on the characteristics of the original data.
2) Data outlier handling
   - This library does not handle outliers and anomaly.
   - Determine the significance of outliers of the dataset and assess whether the removal of outlier is necessary.
3) Ensure there is no null data in each time resolution for the time-series to be downsampled. Data cleaning will be required to post-process of nonsensical data when null data is inputted. 
4) Downsampling library only processes `Double` class, modifications to the code may be required for other data class.
5) Downsampling aggregation follows the left time edge.
   - This downsampling method represents a time interval on the left time edge.
6) Downsampling aggregation follows the left closing.
   - If a data timestamp lands exactly on the edge of a time interval, it falls under the previous/left time interval for downsampling.


