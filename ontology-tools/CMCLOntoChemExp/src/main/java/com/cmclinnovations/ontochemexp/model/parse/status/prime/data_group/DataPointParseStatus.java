package com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group;
/**
 * This class contains getters and setters to flags that maintain whether
 * or not a PrIMe experiment's dataPoint elements and attributes have already 
 * been parsed.
 * 
 * @author Songyi Deng (sd626@cam.ac.uk)
 *
 */
public class DataPointParseStatus {
	boolean dataPoint = false;
	boolean id =false;
	
	public boolean isDataPoint() {
		return dataPoint;
	}
	
	public void setDataPoint(boolean dataPoint) {
		this.dataPoint = dataPoint;
	}
	
	public boolean isID() {
		return id;
	}

	public void setID(boolean id) {
		this.id = id;
	}
	
	int dataPointCount;

	public int getDataPointCount() {
		return dataPointCount;
	}
	public void setDataPointCount(int dataPointCount) {
		this.dataPointCount = dataPointCount;
	}
	

}