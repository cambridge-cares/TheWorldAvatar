package uk.ac.cam.cares.jps.base.region.test;

import static org.junit.Assert.*;

import org.junit.Test;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

public class RegionTest {
	
	
	@Test
	public void testPutRegion() {
		
		JSONObject json = new JSONObject();
		
		// test Singapore-ADMS
		Region.putRegion(json, 1);
		assertEquals(json.get("region").toString(), 
				"{\"uppercorner\":{\"uppery\":\"143305.896\","
								+ "\"upperx\":\"11564077.989\"},"
				+ "\"srsname\":\"EPSG:3857\","
				+ "\"lowercorner\":{\"lowery\":\"140107.739\","
								+ "\"lowerx\":\"11560879.832\"}}");
		
		// test Singapore-Episode
		Region.putRegion(json, 2);
		assertEquals(json.get("region").toString(), 
				"{\"uppercorner\":{\"uppery\":\"151860.32\","
								+ "\"upperx\":\"11572101.89\"},"
				+ "\"srsname\":\"EPSG:3857\","
				+ "\"lowercorner\":{\"lowery\":\"131707.739\","
								+ "\"lowerx\":\"11552101.832\"}}");
		
		// test Hong Kong-ADMS
		Region.putRegion(json, 3);
		assertEquals(json.get("region").toString(), 
				"{\"uppercorner\":{\"uppery\":\"2550426.72\","
								+ "\"upperx\":\"12711879.81\"},"
				+ "\"srsname\":\"EPSG:3857\","
				+ "\"lowercorner\":{\"lowery\":\"2545200.172\","
								+ "\"lowerx\":\"12706653.262\"}}");
		
		// test Hong Kong-Episode
		Region.putRegion(json, 4);
		assertEquals(json.get("region").toString(), 
				"{\"uppercorner\":{\"uppery\":\"2562555.26\","
								+ "\"upperx\":\"12720578.56\"},"
				+ "\"srsname\":\"EPSG:3857\","
				+ "\"lowercorner\":{\"lowery\":\"2534900.06\","
								+ "\"lowerx\":\"12694101.21\"}}");
	}

	@Test
	public void testPutRegionAndStation() {
		
		JSONObject json = new JSONObject();
		
		// test Singapore-ADMS
		Region.putRegionAndStation(json, 1);
		assertEquals(json.get("airStationIRI"), "http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001");		
		
		// test Singapore-Episode
		Region.putRegionAndStation(json, 2);
		assertEquals(json.get("airStationIRI"), "http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-002.owl#AirQualityStation-002");
		
		// test Hong Kong-ADMS
		Region.putRegionAndStation(json, 3);
		assertEquals(json.get("airStationIRI"), "http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-001.owl#AirQualityStation-001");
		
		// test Hong Kong-Episode
		Region.putRegionAndStation(json, 4);
		assertEquals(json.get("airStationIRI"), "http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-002.owl#AirQualityStation-002");
		
	}

    @Test
    public void testGetTargetCRSName() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // agent IRI
        String episodeIRI = "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service";
        String admsIRI = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";

        Region reg = new Region();
        assertNotNull(reg.getClass().getDeclaredMethod("getTargetCRSName", String.class, String.class));
        Method getTargetCRSName = reg.getClass().getDeclaredMethod("getTargetCRSName", String.class, String.class);
        getTargetCRSName.setAccessible(true);

        assertEquals(getTargetCRSName.invoke(reg, episodeIRI, Region.SINGAPORE_IRI),CRSTransformer.EPSG_32648);
        assertEquals(getTargetCRSName.invoke(reg, episodeIRI, Region.HONG_KONG_IRI),CRSTransformer.EPSG_32650);
        assertEquals(getTargetCRSName.invoke(reg, admsIRI, Region.SINGAPORE_IRI),CRSTransformer.EPSG_3414);
        assertEquals(getTargetCRSName.invoke(reg, admsIRI, Region.HONG_KONG_IRI),CRSTransformer.EPSG_2326);
        assertEquals(getTargetCRSName.invoke(reg, admsIRI, Region.BERLIN_IRI),CRSTransformer.EPSG_25833);
        assertEquals(getTargetCRSName.invoke(reg, admsIRI, Region.THE_HAGUE_IRI),CRSTransformer.EPSG_28992);
    }

    @Test
    public void testGetSRTM() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
        Region reg = new Region();
        assertNotNull(reg.getClass().getDeclaredMethod("getSRTM", String.class));
        Method getSRTM = reg.getClass().getDeclaredMethod("getSRTM", String.class);
        getSRTM.setAccessible(true);

        List<String> srtm_sg = new ArrayList<String>();
        srtm_sg.add("N01E103");
        srtm_sg.add("N01E104");

        List<String> srtm_hk = new ArrayList<String>();
        srtm_hk.add("N22E114");

        assertEquals(getSRTM.invoke(reg, Region.SINGAPORE_IRI),srtm_sg);
        assertEquals(getSRTM.invoke(reg, Region.HONG_KONG_IRI),srtm_hk);
    }

}
