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

    private static final String MATCHING_PATH = "/resources/cost_ontobuiltenv.csv";

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

                for (int i = 0; i < buildings.size(); i++) {
                    BuildingInfo building = buildings.get(i);
                    String buildingIri = building.getBuildingIri();
                    int floors = building.getFloors();
                    //unit cost query
                    List<BuildingUsageInfo> usageInfos = building.getBuildingUsageInfo();
                    for (int u = 0; u < usageInfos.size(); u++) {
                        String typeCost = "";
                        String keyCost = "";
                        BuildingUsageInfo usageInfo = usageInfos.get(u);
                    
                        for (int j = 0; j < matchingType.size(); j++){
                            if(usageInfo.getUsage().equals(matchingType.get(j).getenvType()) ){
                                typeCost = matchingType.get(j).getType();
                                // keyCost = matchingType.get(j).getKey();
                                break;
                            }
                        }
                        if(!typeCost.isEmpty()){
                            String costQuery ="SELECT average FROM cost WHERE cost.\"Category\" = '" + typeCost + 
                                            "' AND cost.\"Type\" = 'all'" ;
                            Statement stmtQ = srcConn.createStatement();
                            ResultSet costResult = stmtQ.executeQuery(costQuery);
                            while (costResult.next()) {
                                float cost = costResult.getFloat("average");
                                usageInfos.get(u).setUnitCost(cost);
                            }
                        }
                        
                    }
                    

                    //GFA query
                    String gfaQuery = "SELECT realval AS gfa, cityobject_id\r\n" + //
                                            "FROM citydb.cityobject_genericattrib\r\n" + //
                                            "WHERE attrname = 'GFA' AND cityobject_id = "+ //
                                            "(SELECT cityobject_id\r\n" + //
                                              "FROM citydb.cityobject_genericattrib WHERE strval = '" + buildingIri + "')";
                    Statement stmtGFA = srcConn.createStatement();
                    ResultSet gfaResult = stmtGFA.executeQuery(gfaQuery);
                    float gfa = 0;
                    int cityobject_id = 0;
                    while (gfaResult.next()) {
                        gfa = gfaResult.getFloat("gfa");
                        cityobject_id = gfaResult.getInt("cityobject_id");
                    }

                    //cost calculation
                    if(cityobject_id!=0){
                        float costAll = 0;
                        for(int t = 0; t < usageInfos.size(); t++) {
                            float cost = usageInfos.get(t).getUnitCost() * usageInfos.get(t).getUsageShare() * gfa;
                            costAll = costAll + cost;
                        }
                            
    
                        String costCal = "INSERT INTO citydb.cityobject_genericattrib (cityobject_id, attrname, realval)\n" +
                                        "VALUES (" + cityobject_id + ", 'cost', " + costAll +")\n" +
                                        "ON CONFLICT (cityobject_id, attrname) DO UPDATE SET realval= " + costAll +";";
                        
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
                        .addWhere("?property", "a", "?buildingUsage")
                        .addWhere("?property", "env:hasUsageShare", "?usageshare");

            SelectBuilder sb = new SelectBuilder()
                    .addPrefix("env", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv))
                    .addPrefix("twa", OntologyURIHelper.getOntologyUri(OntologyURIHelper.twa))
                    .addPrefix("rdfs", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdfs))
                    .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                    .addVar("?building").addVar("?usage").addVar("?floor").addVar("?usageshare")
                    .addWhere("?building", "env:hasNumberOfFloors", "?NumberOfFloors")
                    .addWhere("?NumberOfFloors", "env:hasValue", "?floor")
                    .addOptional (usageWB)
                    .addBind("COALESCE(?buildingUsage, 'null')",  "?usage");
                    
            String query = sb.build().toString();
            JSONArray queryResultArray = storeClient.executeQuery(query);

            List<BuildingInfo> buildings = new ArrayList<>();

            for (int i = 0; i < queryResultArray.length(); i++) {
                BuildingInfo building = new BuildingInfo();
                BuildingUsageInfo usageInfo = new BuildingUsageInfo();
                List<BuildingUsageInfo> usageInfos = new ArrayList<>();
                String[] buildingIri = queryResultArray.getJSONObject(i).getString("building").split("Building/");
                String usage = queryResultArray.getJSONObject(i).getString("usage");
                building.setFloors(queryResultArray.getJSONObject(i).getInt("floor"));
                if (!usage.equals("null")){
                    String[] buildingType = usage.split("ontobuiltenv/");
                    usageInfo.setUsge(buildingType[1]);
                    usageInfo.setUsageShare(queryResultArray.getJSONObject(i).getFloat("usageshare"));
                }else {
                    usageInfo.setUsge(usage);
                    usageInfo.setUsageShare(0);
                }   

                if (i > 0){
                    if(!buildingIri[1].equals(buildings.get(buildings.size()-1).getBuildingIri())){
                        building.setBuildingIri(buildingIri[1]);
                        usageInfos.add(usageInfo);
                        building.setBuildingUisageInfo(usageInfos);
                        buildings.add(building);
                    }else{
                        buildings.get(buildings.size()-1).getBuildingUsageInfo().add(usageInfo);
                    }
                }else{
                    building.setBuildingIri(buildingIri[1]);
                    usageInfos.add(usageInfo);
                    building.setBuildingUisageInfo(usageInfos);
                    buildings.add(building);                   
                }                                                                                   
                
            }
            return buildings;
        }catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
    }
}
