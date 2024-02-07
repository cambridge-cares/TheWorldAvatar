package uk.ac.cam.cares.jps.agent.gfaagent;

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
        postgisClient.executeQuery(gfaSQLUpdate);
    }

    private static final String gfaSQLInsert = "INSERT INTO cityobject_genericattrib (attrname, realval, cityobject_id)\n" +
                                              " SELECT 'GFA', building.id AS cityobject_id\n" + 
                                                    "(CASE\n" +
                                                        "WHEN building.storeys_above_ground IS NOT NULL THEN (public.ST_Area(surface_geometry.geometry)*building.storeys_above_ground)\n" +
                                                        "ELSE (building.measured_height/3.2*public.ST_Area(surface_geometry.geometry))\n" +
                                                    "END) AS realval\n" +
                                                    "FROM building, surface_geometry\n" +
                                                    "WHERE building.lod0_footprint_id = surface_geometry.parent_id\n" +
                                                    "WHERE NOT EXISTS (SELECT id FROM cityobject_genericattrib WHERE attrname = 'GFA' AND cityobject_id = subquery.id);";

    private static final String gfaSQLUpdate = "WITH subquery AS (\r\n" + 
                                                "\tSELECT building.id AS id,\r\n" + 
                                                "       (CASE WHEN building.storeys_above_ground IS NOT NULL THEN (public.ST_Area(surface_geometry.geometry)*building.storeys_above_ground)\r\n" + 
                                                "       ELSE (building.measured_height/3.2*public.ST_Area(surface_geometry.geometry)) END) AS GFA\r\n" + 
                                                "       FROM building, surface_geometry\r\n" + 
                                                "       WHERE building.lod0_footprint_id = surface_geometry.parent_id\r\n" + 
                                                ")\r\n" + 
                                                "UPDATE cityobject_genericattrib SET realval = subquery.GFA FROM subquery WHERE attrname = 'GFA' AND cityobject_id = subquery.id;";
                        
}
