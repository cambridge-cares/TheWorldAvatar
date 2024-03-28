package uk.ac.cam.cares.jps.agent.gfaagent;

import java.io.IOException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;

import org.apache.jena.vocabulary.AS;
import org.json.JSONArray;

import com.opencsv.bean.CsvToBeanBuilder;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class CostCalculation {
    private final String dbUrl;
    private final String user;
    private final String password;

    private RemoteRDBStoreClient postgisClient;

    private BuildingInfo buildingInfo;
    private static final String MATCHING_PATH = "/resources/cost_landuse.csv";

    private static final String costSQLInsert = "INSERT INTO cityobject_genericattrib (attrname, realval, cityobject_id)\n" +
    "SELECT DISTINCT ON (attrname, cityobject_id) *\n" +
        "FROM  (\n" +
        "SELECT 'cost',\n" +
        "(CASE WHEN building.storeys_above_ground IS NOT NULL THEN (public.ST_Area(surface_geometry.geometry)*building.storeys_above_ground)\n" +
            "ELSE (building.measured_height/3.2*public.ST_Area(surface_geometry.geometry)) END) AS realval, building.id\n" +
        "FROM building, surface_geometry\n" +
        "WHERE building.lod0_footprint_id = surface_geometry.parent_id\n" +
        ") AS cg(attrname, realval, cityobject_id)\n" +
    "ON CONFLICT (attrname, cityobject_id) DO UPDATE SET realval= cityobject_genericattrib.realval;";
    
    private static final String buildingTypeQuery = "SELECT pl.\"LU_DESC\", cb.id\r\n" + //
                "FROM citydb.building cb, public.landplot pl, citydb.cityobject_genericattrib ccg, public.matched_buildings mb\r\n" + //
                "WHERE cb.id = ccg.cityobject_id \r\n" + //
                "AND ccg.attrname = 'uuid' \r\n" + //
                "AND ccg.strval = mb.building_iri\r\n" + //
                "AND mb.public_landplot_ogc_fid = pl.ogc_fid";

    public CostCalculation (String postgisDb, String postgisUser, String postgisPassword) throws IOException {
        this.dbUrl = postgisDb;
        this.user = postgisUser;
        this.password = postgisPassword;

        this.postgisClient = new RemoteRDBStoreClient(dbUrl, user, password);

        //get matching from csv
        List<MatchingType> matchingType = new CsvToBeanBuilder(new FileReader(MATCHING_PATH))
                .withType(MatchingType.class)
                .build()
                .parse();

        ResultSet buildingType = getBuildingType();
        
        while (buildingType.next()) {
            String typeLand = buildingType.getString("LU_DESC");
            String typeCost = null;
            String keyCost = null;
            for (int i = 0; i < matchingType.size(); i++){
                if(typeLand == matchingType.get(i).getLUType()){
                    typeCost = matchingType.get(i).getType();
                    keyCost = matchingType.get(i).getKey();
                }
            }
            
        }
    }

    public ResultSet getBuildingType(){
        try (Connection srcConn = postgisClient.getConnection()) {
            try (Statement stmt = srcConn.createStatement()) {
                 return stmt.executeQuery(buildingTypeQuery);
            }
        }catch (SQLException e) {
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }  
    }
}
