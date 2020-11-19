package com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group;


import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.Mode;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.eclipse.persistence.oxm.annotations.XmlPath;
/**
 * Holds the structure of the PrIMe DataGroup block.
 * 
 * @author Songyi Deng  (sd626@cam.ac.uk)
 *
 */

@XmlType(propOrder={"id","label","dataPointForm", "dataGroupLink", "property", "dataPoint"})
@XmlAccessorType(XmlAccessType.FIELD)
public class DataGroup{
	@XmlPath("node[@name='dataGroupLink']")
	private DataGroupDataGroupLink dataGroupLink;
	@XmlPath("node[@name='property']")
	private ArrayList<DataGroupProperty> property;
	@XmlPath("node[@name='dataPoint']")
	private ArrayList<DataGroupDataPoint> dataPoint;
	
	public DataGroupDataGroupLink getDataGroupLink() {
		return dataGroupLink;
	}
	public void setDataGroupLink(DataGroupDataGroupLink dataGroupLink) {
		this.dataGroupLink = dataGroupLink;
	}
	
	public ArrayList<DataGroupProperty> getProperty() {
		return property;
	}
	public void setProperty(ArrayList<DataGroupProperty> property) {
		this.property = property;
	}

	public ArrayList<DataGroupDataPoint> getDataPoint() {
		return dataPoint;
	}
	public void setDataPoint(ArrayList<DataGroupDataPoint> dataPoint) {
		this.dataPoint = dataPoint;
	}

	@XmlAttribute
	private String label;
	@XmlAttribute
	private String id;
	@XmlAttribute
	private String dataPointForm;	
	
	public String getLabel() {
		return label;
	}
	public void setLabel(String label) {
		this.label = label;
	}
	
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	
	public String getDataPointForm() {
		return dataPointForm;
	}
	public void setDataPointForm(String dataPointForm) {
		this.dataPointForm = dataPointForm;
	}
}
