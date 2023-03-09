package uk.ac.cam.cares.jps.powsys.nuclear.test;

import static org.junit.Assert.*;
import org.junit.Test;

import uk.ac.cam.cares.jps.powsys.nuclear.NuclearPlantType;

public class TestNuclearPlantType {
	
	// Test NuclearPlantType(String nuclearplanttype), getnuclearplant()
	@Test
	public void testNuclearPlantTypeandgetnuclearplant() {
		String nuclearplanttype = "teststring";
		NuclearPlantType type = new NuclearPlantType(nuclearplanttype);
		assertSame(nuclearplanttype,type.getnuclearplant());
	}
	
	// Test setcapacity(double capacity), getcapacity()
	@Test
	public void testSetcapacityandGetcapacity () {
		double capacity = 123;
		NuclearPlantType type = new NuclearPlantType("");
		type.setcapacity(capacity);
		assertEquals(capacity,type.getcapacity(),0.1);	
	}
	
	// Test setnumberreactora(int numberreactora), setnumberreactorb(int numberreactorb), getnumberreactora(), getnumberreactorb()
	@Test 
	public void testSetnumberreactorandGetnumberreactor() {
		int numberreactora = 10;
		int numberreactorb = 20;
		NuclearPlantType type = new NuclearPlantType("");
		type.setnumberreactora(numberreactora);
		type.setnumberreactorb(numberreactorb);
		assertSame(numberreactora,type.getnumberreactora());
		assertSame(numberreactorb,type.getnumberreactorb());	
	}
	
	// Test getx(), setx(double x), gety(), sety(double y)
    @Test
	public void testGetxSetxGetySety() {
    	NuclearPlantType type = new NuclearPlantType("");
    	double x_value = 123;
		double y_value = 456;
		type.setx(x_value);
		type.sety(y_value);
		assertEquals(x_value,type.getx(),0.1);
		assertEquals(y_value,type.gety(),0.1);
	}
}
