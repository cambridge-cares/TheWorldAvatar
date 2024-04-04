package uk.ac.cam.cares.jps.agent.gfaagent;

import java.io.IOException;
import java.io.FileReader;
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

    
    private static final String buildingTypeQuery = "SELECT pl.\"LU_DESC\", cb.id, mb.building_iri, cb.storeys_above_ground\n" + //
                "FROM citydb.building cb, public.landplot pl, citydb.cityobject_genericattrib ccg, public.matched_buildings mb\r\n" + //
                "WHERE cb.id = ccg.cityobject_id \r\n" + //
                "AND ccg.attrname = 'uuid' \r\n" + //
                "AND ccg.strval = mb.building_iri\r\n" + //
                "AND mb.public_landplot_ogc_fid = pl.ogc_fid";


    public CostCalculation (String postgisDb, String postgisUser, String postgisPassword){
        this.dbUrl = postgisDb;
        this.user = postgisUser;
        this.password = postgisPassword;

        this.postgisClient = new RemoteRDBStoreClient(dbUrl, user, password);
    }

    public void calculationCost () throws IOException {
        //get matching from csv
        List<MatchingType> matchingType = new CsvToBeanBuilder(new FileReader(MATCHING_PATH))
                .withType(MatchingType.class)
                .build()
                .parse();

        try (Connection srcConn = postgisClient.getConnection()) {
            try (Statement stmt = srcConn.createStatement()) {
                ResultSet buildingType = stmt.executeQuery(buildingTypeQuery);
                while (buildingType.next()) {
                    String typeLand = buildingType.getString("LU_DESC");
                    String typeCost = null;
                    String keyCost = null;
                    String buildingIri = buildingType.getString("building_iri");
                    int buildingId = buildingType.getInt("id");
                    int floors = buildingType.getInt("storeys_above_ground");
                    for (int i = 0; i < matchingType.size(); i++){
                        if(typeLand.equals(matchingType.get(i).getLUType()) ){
                            typeCost = matchingType.get(i).getType();
                            keyCost = matchingType.get(i).getKey();
                            break;
                        }
                    } 
                    if(!typeCost.isEmpty() && !keyCost.isEmpty()){
                        String costQuery ="SELECT cost FROM cost WHERE cost.\"Type\" = '" + typeCost + "' AND floorscat > " + floors + 
                                        " union all select MIN(cost) from cost " +//
                                        "where cost.\"" + keyCost + "\" = '" + typeCost + "' AND floorscat IS NULL " + //
                                        "LIMIT 1";

                        String gfaQuery = "SELECT realval AS gfa, cityobject_id\r\n" + //
                                            "FROM citydb.cityobject_genericattrib\r\n" + //
                                            "WHERE attrname = 'GFA' AND cityobject_id= " + buildingId;

                        String costCal = "with cost_table as (" + costQuery + "), gfa_table as ( " + gfaQuery + ")\n" + //
                                    "INSERT INTO citydb.cityobject_genericattrib (cityobject_id, attrname, realval)\n" +
                                    "SELECT DISTINCT ON (cityobject_id, attrname) * FROM(\n" +
                                    "SELECT gfa_table.cityobject_id,'cost', gfa_table.gfa * cost_table.cost\n" +
                                    "FROM gfa_table, cost_table) AS cg(cityobject_id, attrname, realval) " +
                                    "ON CONFLICT (attrname, cityobject_id) DO UPDATE SET realval= cityobject_genericattrib.realval;";
                        
                        Statement stmtupdate = srcConn.createStatement();
                        stmtupdate.executeUpdate(costCal);
                    }
                    
                }
            }
        }catch (SQLException e) {
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
        
    }
}
