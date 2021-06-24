package uk.ac.cam.cares.jps.base.region.test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;

public class ScopeTest {
	
	JSONObject json = new JSONObject();
	Scope SG_ADMS;
	Scope SG_Epi;
	Scope HK_ADMS;
	Scope HK_Epi;
	
	@Before
	public void setUpBefore( ) {
		Region.putRegion(json, 1);
		SG_ADMS = new Scope(json.getJSONObject("region"));
		
		Region.putRegion(json, 2);
		SG_Epi = new Scope(json.getJSONObject("region"));
		
		Region.putRegion(json, 3);
		HK_ADMS = new Scope(json.getJSONObject("region"));
		
		Region.putRegion(json, 4);
		HK_Epi = new Scope(json.getJSONObject("region"));
		
		
	}

	@Test
	public void testGetCRSName() {
		
		assertEquals(SG_ADMS.getCRSName(), "EPSG:3857");
		assertEquals(SG_Epi.getCRSName(), "EPSG:3857");
		assertEquals(HK_ADMS.getCRSName(), "EPSG:3857");
		assertEquals(HK_Epi.getCRSName(), "EPSG:3857");
		
	}

	@Test
	public void testGetScopeCentre() {
		
		assertArrayEquals(SG_ADMS.getScopeCentre(), 
				new double[] {(11560879.832 + 11564077.989) / 2, (140107.739 + 143305.896) / 2}, 1E-3);
		assertArrayEquals(SG_Epi.getScopeCentre(), 
				new double[] {(11552101.832 + 11572101.89) / 2, (131707.739 + 151860.32) / 2}, 1E-3);
		assertArrayEquals(HK_ADMS.getScopeCentre(), 
				new double[] {(12706653.262 + 12711879.81) / 2, (2545200.172 + 2550426.72) / 2}, 1E-3);
		assertArrayEquals(HK_Epi.getScopeCentre(), 
				new double[] {(12694101.21 + 12720578.56) / 2, (2534900.06 + 2562555.26) / 2}, 1E-3);
		
	}

	@Test
	public void testGetUTMzone() {
		
		assertEquals(SG_ADMS.getUTMzone(), "48N");
		assertEquals(SG_Epi.getUTMzone(), "48N");
		assertEquals(HK_ADMS.getUTMzone(), "50N");
		assertEquals(HK_Epi.getUTMzone(), "50N");
		
	}

	@Test
	public void testGetTimeZone() {
		
		assertEquals(SG_ADMS.getTimeZone(), 8);
		assertEquals(SG_Epi.getTimeZone(), 8);
		assertEquals(HK_ADMS.getTimeZone(), 8);
		assertEquals(HK_Epi.getTimeZone(), 8);
		
	}

	@Test
	public void testTransform() {
		
		SG_ADMS.transform("EPSG:4326");
		assertTrue(Math.abs(SG_ADMS.getLowerx() - 103.853151)/103.853151 < 1E-4);
		assertTrue(Math.abs(SG_ADMS.getLowery() - 1.258508)/1.258508 < 1E-4);
		assertTrue(Math.abs(SG_ADMS.getUpperx() - 103.881880)/103.881880 < 1E-4);
		assertTrue(Math.abs(SG_ADMS.getUppery() - 1.287230)/1.287230 < 1E-4);
		
		SG_Epi.transform("EPSG:32648");
		assertTrue(Math.abs(SG_Epi.getLowerx() - 363628.3)/363628.3 < 1E-4);
		assertTrue(Math.abs(SG_Epi.getLowery() - 130794.7)/130794.7 < 1E-4);
		assertTrue(Math.abs(SG_Epi.getUpperx() - 383628.2)/383628.2 < 1E-4);
		assertTrue(Math.abs(SG_Epi.getUppery() - 150794.7)/150794.7 < 1E-4);
		
		// test for wrong case
		try {
			HK_ADMS.transform("EPSG:1326");
		} catch (JPSRuntimeException e) {
			assertEquals(e.getMessage(), "Registry 'epsg' contains no parameter for [EPSG:1326] ");
		}
		
		HK_ADMS.transform("EPSG:4326");
		assertTrue(Math.abs(HK_ADMS.getLowerx() - 114.145808)/114.145808 < 1E-4);
		assertTrue(Math.abs(HK_ADMS.getLowery() - 22.280201)/22.280201 < 1E-4);
		assertTrue(Math.abs(HK_ADMS.getUpperx() - 114.192759)/114.192759 < 1E-4);
		assertTrue(Math.abs(HK_ADMS.getUppery() - 22.323640)/22.323640 < 1E-4);
		
		HK_Epi.transform("EPSG:32650");
		assertTrue(Math.abs(HK_Epi.getLowerx() - 194070.2)/194070.2 < 1E-4);
		assertTrue(Math.abs(HK_Epi.getLowery() - 2457355.8)/2457355.8 < 1E-4);
		assertTrue(Math.abs(HK_Epi.getUpperx() - 219070.2)/219070.2 < 1E-4);
		assertTrue(Math.abs(HK_Epi.getUppery() - 2482355.8)/2482355.8 < 1E-4);
		
	}
	
	@Test
	public void testIsWithinScope() {
		assertTrue(SG_ADMS.isWithinScope(new double[] {1.1562E7, 1.41E5}));
		assertFalse(SG_ADMS.isWithinScope(new double[] {1.6E7, 1.5E5}));
		
		assertTrue(SG_Epi.isWithinScope(new double[] {1.156E7, 1.41E5}));
		assertFalse(SG_Epi.isWithinScope(new double[] {1.6E7, 1.8E5}));
		
		assertTrue(HK_ADMS.isWithinScope(new double[] {1.271E7, 2.55E6}));
		assertFalse(HK_ADMS.isWithinScope(new double[] {1.6E7, 1.5E5}));
		
		assertTrue(HK_Epi.isWithinScope(new double[] {1.27E7, 2.55E6}));
		assertFalse(HK_Epi.isWithinScope(new double[] {1.6E7, 1.5E5}));
		
	}

}
