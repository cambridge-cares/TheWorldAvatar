package uk.ac.cam.cares.jps.base.region.test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class RegionTest extends TestCase {
    /** 
     * This test ensures that the method getTargetCRSName gives the correct CRS for the right combinations of cities and models
     */
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
        srtm_sg.add("N01E103.tif");
        srtm_sg.add("N01E104.tif");

        List<String> srtm_hk = new ArrayList<String>();
        srtm_hk.add("N22E114.tif");

        assertEquals(getSRTM.invoke(reg, Region.SINGAPORE_IRI),srtm_sg);
        assertEquals(getSRTM.invoke(reg, Region.HONG_KONG_IRI),srtm_hk);
    }
}
