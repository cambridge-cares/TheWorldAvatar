package uk.ac.cam.cares.jps.powsys.envisualization.test;

import static org.junit.Assert.*;
import org.junit.Test;

import uk.ac.cam.cares.jps.powsys.envisualization.MapPoint;	 

public class TestMapPoint {				
	   
	@Test
	public void testMapPoint() {
		double lat = 12.3;
		double lon = 45.6;
		double alt = 78.9;
		long time = 100L;
		String name = "nameofpoint";  
		
		// Test MapPoint(double latitude, double longitude)
		MapPoint mapPoint1 = new MapPoint(lat, lon);
		assertEquals(lat,mapPoint1.getLatitude(),0.1);	
		assertEquals(lon,mapPoint1.getLongitude(),0.1);
		
		// Test MapPoint(double latitude, double longitude, double altitude,String nameofpoint)
		MapPoint mapPoint2 = new MapPoint(lat, lon, alt, name);
		assertEquals(lat,mapPoint2.getLatitude(),0.1);	
		assertEquals(lon,mapPoint2.getLongitude(),0.1);
		assertEquals(alt,mapPoint2.getAltitude(),0.1);
		assertSame(name,mapPoint2.getName());
		
		// Test MapPoint(double latitude, double longitude,String nameofpoint)
		MapPoint mapPoint3 = new MapPoint(lat, lon, name);
		assertEquals(lat,mapPoint3.getLatitude(),0.1);	
		assertEquals(lon,mapPoint3.getLongitude(),0.1);
		assertSame(name,mapPoint3.getName());
		
		// Test MapPoint(double latitude, double longitude, double altitude)
		MapPoint mapPoint4 = new MapPoint(lat, lon, alt);
		assertEquals(lat,mapPoint4.getLatitude(),0.1);	
		assertEquals(lon,mapPoint4.getLongitude(),0.1);
		assertEquals(alt,mapPoint4.getAltitude(),0.1);
		
		// Test MapPoint(double latitude, double longitude, double altitude, long time)
		MapPoint mapPoint5 = new MapPoint(lat, lon, alt, time);
		assertEquals(lat,mapPoint5.getLatitude(),0.1);	
		assertEquals(lon,mapPoint5.getLongitude(),0.1);
		assertEquals(alt,mapPoint5.getAltitude(),0.1);
		assertEquals(time,mapPoint5.getTime(),0.1);
		
		// Test MapPoint(double latitude, double longitude, double altitude, long time, String name)
		// Test getLatitude(), getLongitude(), getAltitude(), getTime(), getName()
		MapPoint mapPoint6 = new MapPoint(lat, lon, alt, time, name);
		assertEquals(lat,mapPoint6.getLatitude(),0.1);	
		assertEquals(lon,mapPoint6.getLongitude(),0.1);
		assertEquals(alt,mapPoint6.getAltitude(),0.1);
		assertEquals(time,mapPoint6.getTime(),0.1);
		assertSame(name,mapPoint6.getName());
		
		// Test setName(String name)
		mapPoint6.setName("TestName");
		assertSame("TestName",mapPoint6.getName());
		
		// Test setTime(long time)
		mapPoint6.setTime(200L);
		assertEquals(200L,mapPoint6.getTime(),0.1);
		
		// Test toString()
		String testString = "TestName" + ": " + lat + ", " + lon + ", " + alt + ", " + 200L;
		assertEquals(testString,mapPoint6.toString());		
	} 
}		