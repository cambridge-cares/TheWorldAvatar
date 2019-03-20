package uk.ac.cam.cares.jps.powsys.nuclear;

public class CSVdata {
	
	private String maindata = "";
	private int index = 0;
	
	public CSVdata(String maindata) {
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
