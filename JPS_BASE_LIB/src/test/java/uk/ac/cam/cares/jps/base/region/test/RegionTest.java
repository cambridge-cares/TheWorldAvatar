package uk.ac.cam.cares.jps.base.region.test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.Test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class RegionTest extends TestCase {
    @Test
    public void testGetTargetCRSName() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        /** 
         * This test ensures that the method getTargetCRSName gives the correct CRS for the right combinations of cities and models
         */

        // City IRIs
        String BERLIN_IRI = "http://dbpedia.org/resource/Berlin";
        String THE_HAGUE_IRI = "http://dbpedia.org/resource/The_Hague";
        String SINGAPORE_IRI = "http://dbpedia.org/resource/Singapore";
        String HONG_KONG_IRI = "http://dbpedia.org/resource/Hong_Kong";

        // agent IRI
        String episodeIRI = "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service";
        String admsIRI = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";

        Region reg = new Region();
        assertNotNull(reg.getClass().getDeclaredMethod("getTargetCRSName", String.class, String.class));
        Method getTargetCRSName = reg.getClass().getDeclaredMethod("getTargetCRSName", String.class, String.class);
        getTargetCRSName.setAccessible(true);

        assertEquals(getTargetCRSName.invoke(reg, episodeIRI, SINGAPORE_IRI),CRSTransformer.EPSG_32648);
        assertEquals(getTargetCRSName.invoke(reg, episodeIRI, HONG_KONG_IRI),CRSTransformer.EPSG_32650);
        assertEquals(getTargetCRSName.invoke(reg, admsIRI, SINGAPORE_IRI),CRSTransformer.EPSG_3414);
        assertEquals(getTargetCRSName.invoke(reg, admsIRI, HONG_KONG_IRI),CRSTransformer.EPSG_2326);
        assertEquals(getTargetCRSName.invoke(reg, admsIRI, BERLIN_IRI),CRSTransformer.EPSG_25833);
        assertEquals(getTargetCRSName.invoke(reg, admsIRI, THE_HAGUE_IRI),CRSTransformer.EPSG_28992);
    }
}
