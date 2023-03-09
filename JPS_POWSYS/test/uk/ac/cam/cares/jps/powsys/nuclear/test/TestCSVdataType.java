package uk.ac.cam.cares.jps.powsys.nuclear.test;

import static org.junit.Assert.*;
import org.junit.Test;	

import uk.ac.cam.cares.jps.powsys.nuclear.CSVdataType;

public class TestCSVdataType {
	
	// Test CSVdataType(String maindata), getmaindata() 
	@Test 
	public void testCSVdataTypeandGetmaindata() {
		String maindata = "teststring";
		CSVdataType type = new CSVdataType(maindata);
		assertSame(maindata,type.getmaindata());
	}
	
	// Test getindex(), setindex(int index)
	@Test
	public void testGetindexandSetindex() {
		CSVdataType type = new CSVdataType("");
		int index = 10;
		type.setindex(index);
		assertSame(index,type.getindex());
	}

}
