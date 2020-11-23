package com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder={ "dataGroupID", "dataPointID" })
@XmlAccessorType(XmlAccessType.FIELD)
public class DataGroupDataGroupLink {
	@XmlAttribute
	private String dataGroupID;
	@XmlAttribute
	private String dataPointID;
	
	public String getDataGroupID() {
		return dataGroupID;
	}

	public void setDataGroupID(String dataGroupID) {
		this.dataGroupID = dataGroupID;
	}

	public String getDataPointID() {
		return dataPointID;
	}
	
	public void setDataPointID(String dataPointID) {
		this.dataPointID = dataPointID;		
	}
	
	@XmlValue
	private String value;
	
	public String getValue() {
		return value;
	}
	
	public void setValue(String value) {
		this.value = value;
	}
}
