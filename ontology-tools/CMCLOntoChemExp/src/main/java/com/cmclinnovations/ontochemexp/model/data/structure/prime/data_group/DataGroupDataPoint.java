package com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.DataGroupDataPointX;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X1;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X10;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X11;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X2;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X3;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X4;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X5;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X6;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X7;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X8;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X9;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

@XmlAccessorType(XmlAccessType.FIELD)
public class DataGroupDataPoint {
	@XmlAttribute
	private String id;
	
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	
	@XmlElement
	private X1 x1;
	public X1 getX1() {
		return x1;
	}
	public void setX1(X1 x1) {
		this.x1 = x1;
	}
	
	@XmlElement
	private X2 x2;
	public X2 getX2() {
		return x2;
	}
	public void setX2(X2 x2) {
		this.x2 = x2;
	}
	
	@XmlElement
	private X3 x3;
	public X3 getX3() {
		return x3;
	}
	public void setX3(X3 x3) {
		this.x3 = x3;
	}
	
	@XmlElement
	private X4 x4;
	public X4 getX4() {
		return x4;
	}
	public void setX4(X4 x4) {
		this.x4 = x4;
	}
	
	@XmlElement
	private X5 x5;
	public X5 getX5() {
		return x5;
	}
	public void setX5(X5 x5) {
		this.x5 = x5;
	}
	
	@XmlElement
	private X6 x6;
	public X6 getX6() {
		return x6;
	}
	public void setX6(X6 x6) {
		this.x6 = x6;
	}
	
	@XmlElement
	private X7 x7;
	public X7 getX7() {
		return x7;
	}
	public void setX7(X7 x7) {
		this.x7 = x7;
	}
	
	@XmlElement
	private X8 x8;
	public X8 getX8() {
		return x8;
	}
	public void setX8(X8 x8) {
		this.x8 = x8;
	}
	
	@XmlElement
	private X9 x9;
	public X9 getX9() {
		return x9;
	}
	public void setX9(X9 x9) {
		this.x9 = x9;
	}
	
	@XmlElement
	private X10 x10;
	public X10 getX10() {
		return x10;
	}
	public void setX10(X10 x10) {
		this.x10 = x10;
	}
	
	@XmlElement
	private X11 x11;
	public X11 getX11() {
		return x11;
	}
	public void setX11(X11 x11) {
		this.x11 = x11;
	}
}
