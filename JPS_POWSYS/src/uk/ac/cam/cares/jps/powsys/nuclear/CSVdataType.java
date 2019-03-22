package uk.ac.cam.cares.jps.powsys.nuclear;

public class CSVdataType {
	
	private String maindata = "";
	private int index = 0;
	
	public CSVdataType(String maindata) {
		this.maindata = maindata;
	}
	
	public String getmaindata() {
		return maindata;
	}
	
	public int getindex() {
		return index;
	}
	
	public void setindex(int index) {
		this.index = index;
	}
}
