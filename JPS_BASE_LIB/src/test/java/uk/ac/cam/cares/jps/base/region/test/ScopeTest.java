package uk.ac.cam.cares.jps.base.region.test;

import static org.junit.Assert.assertEquals;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;

public class ScopeTest {
    @Test
    public void testGetUTMzone() {
        /** 
         *  Ensure the correct UTM zone is returned
         */
        JSONObject jo1 = new JSONObject();
        Region.putRegion(jo1, 1);
        Scope sc1 = new Scope(jo1.getJSONObject(Region.keyRegion)); // Singapore scope

        JSONObject jo2 = new JSONObject();
        Region.putRegion(jo2, 3);
        Scope sc2 = new Scope(jo2.getJSONObject(Region.keyRegion)); // Hong Kong scope

        assertEquals(sc1.getUTMzone(),"48N");
        assertEquals(sc2.getUTMzone(),"50N");
    }

    @Test
    public void testGetTimeZone() {
        JSONObject jo1 = new JSONObject();
        Region.putRegion(jo1, 1);
        Scope sc1 = new Scope(jo1.getJSONObject(Region.keyRegion)); // Singapore scope

        JSONObject jo2 = new JSONObject();
        Region.putRegion(jo2, 3);
        Scope sc2 = new Scope(jo2.getJSONObject(Region.keyRegion)); // Hong Kong scope

        assertEquals(sc1.getTimeZone(),8);
        assertEquals(sc2.getTimeZone(),8);
    }
}
