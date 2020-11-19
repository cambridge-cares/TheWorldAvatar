package com.cmclinnovations.ontochemexp.model.parse.status.prime;

/**
 * This class contains getters and setters to flags that maintain whether or not
 * a PrIMe experiment's dataGroup elements and attributes have already been
 * parsed.
 * 
 * @author Songyi Deng (sd626@cam.ac.uk)
 *
 */
public class DataGroupParseStatus {
	boolean dataGroup = false;
	boolean id = false;
	boolean label = false;
	boolean dataPointForm = false;

	public boolean isDataGroup() {
		return dataGroup;
	}

	public void setDataGroup(boolean dataGroup) {
		this.dataGroup = dataGroup;
	}

	public boolean isID() {
		return id;
	}

	public void setID(boolean id) {
		this.id = id;
	}

	public boolean isLabel() {
		return label;
	}

	public void setLabel(boolean label) {
		this.label = label;
	}

	public boolean isDataPointForm() {
		return dataPointForm;
	}

	public void setDataPointForm(boolean dataPointForm) {
		this.dataPointForm = dataPointForm;
	}

	int dataGroupCount;

	public int getDataGroupCount() {
		return dataGroupCount;
	}

	public void setDataGroupCount(int dataGroupCount) {
		this.dataGroupCount = dataGroupCount;
	}
}