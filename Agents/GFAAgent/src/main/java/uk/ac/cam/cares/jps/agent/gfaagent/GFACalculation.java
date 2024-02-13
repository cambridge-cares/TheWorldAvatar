package uk.ac.cam.cares.jps.agent.gfaagent;

import org.apache.jena.vocabulary.AS;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

public class GFACalculation {

    private final String dbUrl;
    private final String user;
    private final String password;

    private RemoteRDBStoreClient postgisClient;

    public GFACalculation (String postgisDb, String postgisUser, String postgisPassword){
        this.dbUrl = postgisDb;
        this.user = postgisUser;
        this.password = postgisPassword;

        this.postgisClient = new RemoteRDBStoreClient(dbUrl, user, password);
    }
    
    public void calculationGFA(){
        postgisClient.executeQuery(gfaSQLInsert);
    }

    private static final String gfaSQLInsert = "INSERT INTO cityobject_genericattrib (attrname, realval, cityobject_id)\n" +
                                                "SELECT DISTINCT ON (attrname, cityobject_id) *\n" +
                                                    "FROM  (\n" +
                                                    "SELECT 'GFA',\n" +
                                                    "(CASE WHEN building.storeys_above_ground IS NOT NULL THEN (public.ST_Area(surface_geometry.geometry)*building.storeys_above_ground)\n" +
                                                        "ELSE (building.measured_height/3.2*public.ST_Area(surface_geometry.geometry)) END) AS realval, building.id\n" +
                                                    "FROM building, surface_geometry\n" +
                                                    "WHERE building.lod0_footprint_id = surface_geometry.parent_id\n" +
                                                    ") AS cg(attrname, realval, cityobject_id)\n" +
                                                "ON CONFLICT (attrname, cityobject_id) DO UPDATE SET realval= cityobject_genericattrib.realval;";

   
}
