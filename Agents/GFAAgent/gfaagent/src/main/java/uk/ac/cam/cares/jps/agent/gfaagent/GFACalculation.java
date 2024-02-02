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
        JSONArray result = postgisCliente.executeQuery(gfaSQLQuery);
        if (!result.isEmpty()) {

        }
    }
    private static final String gfaSQLQuery = "SELECT building.id, cityobject_genericattrib.urival AS iri,\n" + 
                                                    "(CASE\n" +
                                                        "WHEN building.storeys_above_ground IS NOT NULL THEN (public.ST_Area(surface_geometry.geometry)*building.storeys_above_ground)\n" +
                                                        "ELSE (building.measured_height/3.2*public.ST_Area(surface_geometry.geometry))\n" +
                                                    "END) AS GFA\n" +
                                                    "FROM building, surface_geometry, cityobject_genericattrib\n" +
                                                    "WHERE building.lod0_footprint_id = surface_geometry.parent_id  AND building.id = cityobject_genericattrib.cityobject_id AND cityobject_genericattrib.attrname = 'iri'";
}
