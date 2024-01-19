package uk.ac.cam.cares.jps.powsys.nuclear.test;

import static org.junit.Assert.*;
import org.junit.Test;

import uk.ac.cam.cares.jps.powsys.nuclear.NuclearGenType;

public class TestNuclearGenType {
	
	// Test NuclearGenType(String NuclearGenType), getnucleargen()
	@Test
	public void testNuclearGenTypeandgetnucleargen() {
		String nuclearGenType = "teststring";
		NuclearGenType type = new NuclearGenType(nuclearGenType);
		assertSame(nuclearGenType,type.getnucleargen());
	}
	
	// Test setcapacity(double capacity), getcapacity()
	@Test
	public void testSetcapacityandGetcapacity() {
		double capacity = 123;
		NuclearGenType type = new NuclearGenType("");
		type.setcapacity(capacity);
		assertEquals(capacity,type.getcapacity(),0.1);
	}
	
	// Test setid(String id), getid()
	@Test
	public void testSetidandGetid() {
		String string_id = "testID";
		NuclearGenType type = new NuclearGenType("");
		type.setid(string_id);
		assertSame(string_id,type.getid());
	}
}
