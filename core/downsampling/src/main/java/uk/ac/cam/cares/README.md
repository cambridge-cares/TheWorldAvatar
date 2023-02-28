# Downsampling
## When to downsample a dataset
- Reduce the size of data to prevent memory occupation of database.
- Duplicated data within short time interval that takes up space but produces no value. This increases the query response time as large dataset will take longer time to response.

## Areas of consideration
1) Downsampled data should be an accurate representation of the inherent nature/characteristics of the original data 
   - Select the right choice of downsampling method
2) Purpose and preservation of information
   - For visualization of trends?
   - For statistical analysis to extract insights?
3) Outlier handling
   - Significance of outliers – Should it be included or excluded?
   - Will it affect the accuracy of representation if not removed? 
4) Downsampling resolution size

# User Guide 
## Description
This class
## How to use downsampling method
``` 
Aggregation (TimeSeries ts, Long resolution, int type)
```
`ts` is the raw timeseries data that will be downsampled.

`resolution` is the time interval in number of seconds that will be downsampled. `resolution` is in Long type. 

`type` is the type of [downsampling method](#Downsampling Type). Input the number 



#### Assumption
1) Downsampling aggregation follows the left edge.
- For a time interval, this downsampling will represents this time interval on the left time edge.
2) Downsampling aggregation follows the left closing.
- For a time interval, this downsampling method will close left. For example, if a data timestamp lands exactly on the edge of a time interval, it will fall under the previous/left time interval for downsampling. 

## Downsampling Type
### 1) Maximum
### 2) Median
### 3) Minimum
### 4) Sum 
### 5) Average
### 6) Count
### 7) Instantaneous



## Output 

## Notes 
1) Be sure to clean your data 