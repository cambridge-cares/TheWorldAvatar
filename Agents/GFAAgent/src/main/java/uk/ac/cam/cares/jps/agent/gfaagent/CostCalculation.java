package uk.ac.cam.cares.jps.agent.gfaagent;

import java.io.IOException;
import java.io.FileReader;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.ArrayList;

import org.apache.jena.vocabulary.AS;
import org.json.JSONArray;

import com.opencsv.bean.CsvToBeanBuilder;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import org.apache.jena.query.Query;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.lang.sparql_11.ParseException;

public class CostCalculation {
    private final String dbUrl;
    private final String user;
    private final String password;

    private RemoteRDBStoreClient postgisClient;
    private String ontopUrl;

    // private BuildingInfo buildingInfo;
    private static final String MATCHING_PATH = "/resources/cost_ontobuiltenv.csv";

    
    // private static final String buildingTypeQuery = "SELECT pl.\"LU_DESC\", cb.id, mb.building_iri, cb.storeys_above_ground\n" + //
    //             "FROM citydb.building cb, public.landplot pl, citydb.cityobject_genericattrib ccg, public.matched_buildings mb\r\n" + //
    //             "WHERE cb.id = ccg.cityobject_id \r\n" + //
    //             "AND ccg.attrname = 'uuid' \r\n" + //
    //             "AND ccg.strval = mb.building_iri\r\n" + //
    //             "AND mb.public_landplot_ogc_fid = pl.ogc_fid";


    public CostCalculation (String postgisDb, String postgisUser, String postgisPassword, String ontopUrl){
        this.dbUrl = postgisDb;
        this.user = postgisUser;
        this.password = postgisPassword;

        this.postgisClient = new RemoteRDBStoreClient(dbUrl, user, password);
        this.ontopUrl = ontopUrl;
    }

    public void calculationCost () throws IOException {
        //get matching from csv
        List<MatchingType> matchingType = new CsvToBeanBuilder(new FileReader(MATCHING_PATH))
                .withType(MatchingType.class)
                .build()
                .parse();

        List<BuildingInfo> buildings = queryBuildingUsage();
        try (Connection srcConn = postgisClient.getConnection()) {
            try (Statement stmt = srcConn.createStatement()) {
                // ResultSet buildingType = stmt.executeQuery(buildingTypeQuery);
                for (int i = 0; i < buildings.size(); i++) {
                    BuildingInfo building = buildings.get(i);
                    String buildingIri = building.getBuildingIri();
                    String buildingUsage = building.getType();
                    int floors = building.getFloors();
                    String typeCost = "";
                    String keyCost = "";
                    
                    for (int j = 0; j < matchingType.size(); j++){
                        if(buildingUsage.equals(matchingType.get(j).getenvType()) ){
                            typeCost = matchingType.get(j).getType();
                            keyCost = matchingType.get(j).getKey();
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
                                            "WHERE attrname = 'GFA' AND strval = '" + buildingIri + "'";

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

    public List<BuildingInfo> queryBuildingUsage () {
    
        StringBuilder contentBuilder = new StringBuilder();
        
        try {
            RemoteStoreClient storeClient = new RemoteStoreClient(this.ontopUrl);

            WhereBuilder usageWB = new WhereBuilder()
                        .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                        .addPrefix("env", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv))
                        .addWhere("?building", "env:hasPropertyUsage", "?property")
                        .addWhere("?property", "a", "?buildingUsage");

            SelectBuilder sb = new SelectBuilder()
                    .addPrefix("env", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv))
                    .addPrefix("twa", OntologyURIHelper.getOntologyUri(OntologyURIHelper.twa))
                    .addPrefix("rdfs", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdfs))
                    .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                    .addVar("?building").addVar("?usage").addVar("?floor")
                    .addWhere("?building", "env:hasNumberOfFloors", "?NumberOfFloors")
                    .addWhere("?NumberOfFloors", "env:hasValue", "?floor")
                    .addOptional (usageWB)
                    .addBind("COALESCE(?buildingUsage, 'null')",  "?usage");
                    
            String query = sb.build().toString();
            JSONArray queryResultArray = storeClient.executeQuery(query);

            List<BuildingInfo> buildings = new ArrayList<>();

            for (int i = 0; i < queryResultArray.length(); i++) {
                BuildingInfo building = new BuildingInfo();
                String[] buildingIri = queryResultArray.getJSONObject(i).getString("building").split("Building/");
                building.setBuildingIri(buildingIri[1]);
                String usage = queryResultArray.getJSONObject(i).getString("usage");
                if (!usage.equals("null")){
                    String[] buildingType = usage.split("ontobuiltenv/");
                    building.setType(buildingType[1]);
                }else {
                    building.setType(usage);
                }                                              
                building.setFloors(queryResultArray.getJSONObject(i).getInt("floor"));
                buildings.add(building);
            }
            return buildings;
        }catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
    }
}
