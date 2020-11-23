package com.cmclinnovations.ontochemexp.model.data.structure.prime.property;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "id", "primeID", "type", "indicator", "observable" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Feature {
	@XmlAttribute
	private String id;
	@XmlAttribute
	private String primeID;
	@XmlAttribute
	private String type;
	
	public String getFeatureId() {
		return id;
	}
	public void setFeatureId(String id) {
		this.id = id;
	}
	
	public String getFeaturePrimeID() {
		return primeID;
	}
	public void setFeaturePrimeID(String primeID) {
		this.primeID = primeID;
	}
	
	public String getFeatureType() {
		return type;
	}
	public void setFeatureType(String type) {
		this.type = type;
	}
	
	@XmlElement
	private ArrayList<Indicator> indicator;
	@XmlElement
	private Observable observable;
	
	public ArrayList<Indicator> getFeatureIndicator() {
		return indicator;
	}
	public void setFeatureIndicator(ArrayList<Indicator> indicator) {
		this.indicator = indicator;
	}
	
	public Observable getFeatureObservable() {
		return observable;
	}
	public void setFeatureObservable(Observable observable) {
		this.observable = observable;
	}
}
