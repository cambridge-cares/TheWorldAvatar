package uk.ac.ceb.como.molhub.bean;

public class RotationalConstant {

	private String rotationalConstantsSize;	
	private String rotationalConstantsValue;
	private String rotationalConstantsUnit;
	
	public RotationalConstant(String rotationalConstantsSize,String rotationalConstantsValue,String rotationalConstantsUnit) {
		
		this.rotationalConstantsSize=rotationalConstantsSize;
		this.rotationalConstantsValue=rotationalConstantsValue;
		this.rotationalConstantsUnit=rotationalConstantsUnit;
	}
	
	public String getRotationalConstantsSize() {
		return rotationalConstantsSize;
	}
	public void setRotationalConstantsSize(String rotationalConstantsSize) {
		this.rotationalConstantsSize = rotationalConstantsSize;
	}
	public String getRotationalConstantsUnit() {
		return rotationalConstantsUnit;
	}
	public void setRotationalConstantsUnit(String rotationalConstantsUnit) {
		this.rotationalConstantsUnit = rotationalConstantsUnit;
	}
	public String getRotationalConstantsValue() {
		return rotationalConstantsValue;
	}
	public void setRotationalConstantsValue(String rotationalConstantsValue) {
		this.rotationalConstantsValue = rotationalConstantsValue;
	}
	
	
}
