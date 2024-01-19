package uk.ac.cam.cares.jps.powsys.nuclear.test;

import static org.junit.Assert.*;
import org.junit.Test;

import uk.ac.cam.cares.jps.powsys.nuclear.LandlotObjectType;	

public class TestLandlotObjectType {
	
	// Test LandlotObjectType(String Landlot), getlot()
	@Test
	public void testLandlotObjectType() {
		String Landlot = "teststring";
		LandlotObjectType type = new LandlotObjectType(Landlot);
		assertSame(Landlot, type.getlot());	
	}

	// Test getx(), setx(double x), gety(), sety(double y)
	@Test
	public void testGetxSetxGetySety() {
		LandlotObjectType type = new LandlotObjectType("");
		double x_value = 123;
		double y_value = 456;
		type.setx(x_value);
		type.sety(y_value);
		assertEquals(x_value,type.getx(),0.1);
		assertEquals(y_value,type.gety(),0.1);
		
	}
}
