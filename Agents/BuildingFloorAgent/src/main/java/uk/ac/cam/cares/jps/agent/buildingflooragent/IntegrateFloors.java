package uk.ac.cam.cares.jps.agent.buildingflooragent;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Map;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.List;
import java.util.ArrayList;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import com.intuit.fuzzymatcher.component.MatchService;
import com.intuit.fuzzymatcher.domain.Document;
import com.intuit.fuzzymatcher.domain.Element;
import com.intuit.fuzzymatcher.domain.ElementType;
import com.intuit.fuzzymatcher.domain.Match;

import com.opencsv.bean.CsvToBeanBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.lang.sparql_11.ParseException;

public class IntegrateFloors {

    private final String dbUrl;
    private final String user;
    private final String password;
    private String osmSchema;
    private String osmPoint;
    private String osmPolygon;
    private String ontopUrl;

    private RemoteRDBStoreClient postgisClient;
    private List<OSMBuilding> osmBuildings = new ArrayList<>();
    
    public IntegrateFloors (String postgisDb, String postgisUser, String postgisPassword, String osmSchema, String osmPoint, String osmPolygon, String ontopUrl){
        this.dbUrl = postgisDb;
        this.user = postgisUser;
        this.password = postgisPassword;
        this.postgisClient = new RemoteRDBStoreClient(dbUrl, user, password);

        this.osmSchema = osmSchema;
        this.osmPoint = osmPoint;
        this.osmPolygon = osmPolygon;

        this.ontopUrl = ontopUrl;
    }

    //check table building has floor Cat. column
    public void addFloorCatColumn () {
        String buildingSQLAlter = "ALTER TABLE building ADD COLUMN IF NOT EXISTS storeys_above_ground_cat character varying(4000);";
        try (Connection srcConn = this.postgisClient.getConnection()) {
            try (Statement stmt = srcConn.createStatement()) {
                stmt.executeUpdate(buildingSQLAlter);
            }
        }catch (SQLException e) {
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }  
    }


    /******************************************** */
    /* Fuzzy match building address from outer data source (csv) and osm agent to integrate floors data to citydb.buildilng */
    /* INPUT: data file location*/
    /* floors data store in citydb.building.storeys_above_ground*/
    /**********************************************/ 
    public void matchAddress (String floorsCsv) throws IOException{
        MatchService matchService = new MatchService();
        //query address infor from osm db   
        this.osmBuildings = queryOSMBuilding();
        List<Document> preDoc = new ArrayList<>();
    
        try (Connection srcConn = postgisClient.getConnection()) {
            //construct preDocument (osm data)
            for (int i = 0; i < this.osmBuildings.size(); i++){
                String buildingiri = osmBuildings.get(i).getBuildingIri();
                String osmStreet = osmBuildings.get(i).getStreet();
                String osmUnit = osmBuildings.get(i).getUnit();
                Document preDocument = new Document.Builder(buildingiri)
                .addElement(new Element.Builder<String>().setValue(osmUnit).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                            .addElement(new Element.Builder<String>().setValue(osmStreet).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                            .createDocument();
                preDoc.add(preDocument);
            }

                //get data from csv
                List<FloorsCsv> hdbFloors = new CsvToBeanBuilder(new FileReader(floorsCsv))
                        .withType(FloorsCsv.class)
                        .build()
                        .parse();


                //fuzzy match
                for (int i = 0; i < hdbFloors.size(); i++){
                    double polyScore = 0.0;
                    double pointScore = 0.0;
                    String blk =  hdbFloors.get(i).getBLK();
                    String address = hdbFloors.get(i).getStreet();

                    Document matchDoc = new Document.Builder(String.valueOf(i))
                            .addElement(new Element.Builder<String>().setValue(blk).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                            .addElement(new Element.Builder<String>().setValue(address).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                            .setThreshold(0.5).createDocument();
                    
                    Map<String, List<Match<Document>>> resultPoint = matchService.applyMatchByDocId(matchDoc,preDoc);

                    String pointIri = null;
                    for (Map.Entry<String, List<Match<Document>>> entry : resultPoint.entrySet()) {
                        for (Match<Document> match : entry.getValue()) {
                            if(match.getScore().getResult()>pointScore && match.getScore().getResult()>0.5){
                                System.out.println("Data: " + match.getData() + " Matched With: " + match.getMatchedWith() + " Score: " + match.getScore().getResult());
                                pointScore = match.getScore().getResult();
                                pointIri = match.getMatchedWith().getKey();
                            }
                        }
                    }
                    
                    //store floors data based on building iri from osm agent
                    Integer floors = hdbFloors.get(i).getFloors();
                    String catString = "A";
                    String buildingiri = null;
                    if(pointScore != 0) {
                        buildingiri = pointIri;
                    }

                    if (buildingiri != null) {
                        // updateFloors(floors, catString, buildingiri);
                    }
                    
                }
            
        } catch (SQLException e) {
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }       
    }

    public void importFloorDate () {
        String catString = null;
        String floorSQLQuery = "SELECT storeys_above_ground, storeys_above_ground_cat, building.id, cg.strval " +
                                "FROM citydb.building, citydb.cityobject_genericattrib cg " + 
                                "WHERE building.id = cg.cityobject_id AND cg.attrname = 'uuid'";       
        int floors;
        try (Connection srcConn = postgisClient.getConnection()) {
            try (Statement stmt = srcConn.createStatement()) {
                ResultSet floorsResults = stmt.executeQuery(floorSQLQuery);
                while (floorsResults.next()) {
                    floors = floorsResults.getInt("storeys_above_ground");
                    catString = floorsResults.getString("storeys_above_ground_cat");
                    String buildingIri = floorsResults.getString("strval");
                    if (floors == 0 || catString.equals("C") || catString == null){// get osm floor
                        floors = queryOSMFloor(buildingIri);
                        if (floors > 0){
                            catString = "B";
                        }else {//estimate
                            catString = "C";
                            floors = estimateFloors(buildingIri);
                        }
                    }
                    if (floors > 0) {
                        updateFloors(floors, catString, buildingIri);
                    }
                    
                }
                
            }
        }catch (SQLException e) {
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
    }

    public int queryOSMFloor (String buildingIri) {
        String osmFloorQuery = "SELECT building_levels FROM osm.polygons " + 
                                "WHERE building_iri = '" + buildingIri + "'";
        int floors = 0;
        try (Connection srcConn = postgisClient.getConnection()) {
            try (Statement stmtOSM = srcConn.createStatement()) {
                ResultSet floorsResults = stmtOSM.executeQuery(osmFloorQuery);
                while (floorsResults.next()) {
                    floors = floorsResults.getInt("building_levels");
                }

            }
            return floors;
        }catch (SQLException e) {
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
    }

    public void updateFloors (Integer floors, String catString, String buildingIri) {
        try (Connection srcConn = postgisClient.getConnection()) {
            try (Statement stmt = srcConn.createStatement()) {
                String buildingSQLUpdate = "UPDATE citydb.building b SET storeys_above_ground = " + floors + 
                                            ", storeys_above_ground_cat = '" + catString +
                                            "' FROM citydb.cityobject_genericattrib cg\n" + 
                                            "WHERE b.id = cg.cityobject_id AND cg.strval = '" + buildingIri + "';";
                postgisClient.executeUpdate(buildingSQLUpdate);

            }
        }catch (SQLException e) {
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
        

    }

    /********************************************************* */
    /*To estimate calculate height: */
    /*1. query building usage: Domestic: 1st floor 3.6m, the others 2.8m/floor; Non-Domestic: 3.3m/floor */
    /*2. no usage information: 3.3m/floor */
    /*********************************************************** */
    public Integer estimateFloors (String buildingIri) {
        
        try {
            int floor = 0;
            float height = 0;

            String heightQuery = "SELECT measured_height" +
                                " FROM citydb.building, citydb.cityobject_genericattrib cg " + 
                                "WHERE building.id = cg.cityobject_id AND cg.attrname = 'uuid' AND cg.strval = '" + buildingIri + "'";
            try (Connection srcConn = postgisClient.getConnection()) {
                try (Statement stmt = srcConn.createStatement()) {
                    ResultSet heightResults = stmt.executeQuery(heightQuery);
                    while (heightResults.next()) {
                        height = heightResults.getFloat("measured_height");
                    }        
                }
            }catch (SQLException e) {
                throw new JPSRuntimeException("Error connecting to source database: " + e);
            }

            for (int i = 0; i< this.osmBuildings.size(); i++){
                if(buildingIri.equals(this.osmBuildings.get(i).getBuildingIri())){
                    String[] usage = this.osmBuildings.get(i).getUsage().split("ontobuiltenv/", 2);
                    if (usage[1].equals("Domestic") || usage[1].contains("Residential")){
                        floor = (int) ((height-3.6) / 2.8 + (((height-3.6) % 2.8 == 0) ? 0 : 1)) + 1;
                        break;
                    } 
                }
            }

            if (floor == 0 && height > 0) {
                floor =(int) (height / 3.3 + ((height % 3.3 == 0) ? 0 : 1));
            }

            return floor;
        }catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }

    }

    public List<OSMBuilding> queryOSMBuilding () {
        String fileContent = "";
        StringBuilder contentBuilder = new StringBuilder();
        
        try {
            RemoteStoreClient storeClient = new RemoteStoreClient(this.ontopUrl);

            SelectBuilder sb = new SelectBuilder()
                    .addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.om))
                    .addPrefix("env", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv))
                    .addPrefix("twa", OntologyURIHelper.getOntologyUri(OntologyURIHelper.twa))
                    .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                    .addPrefix("rdfs", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdfs))
                    .addPrefix("ic", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ic))
                    .addVar("?building").addVar("?usage").addVar("?street").addVar("?postcode").addVar("?unit")
                    .addWhere("?building", "env:hasPropertyUsage", "?property")
                    .addWhere("?property", "a", "?usage")
                    .addWhere("?building", "env:hasAddress", "?address")
                    .addOptional("?address", "ic:hasStreet", "?streetName")
                    .addOptional("?address", "ic:hasPostalCode", "?postCode")
                    .addOptional("?address", "ic:hasUnitNumber", "?unitNum")
                    .addBind("COALESCE(?streetName, 'null')",  "?street")
                    .addBind("COALESCE(?postCode, 'null')", "?postcode")
                    .addBind("COALESCE(?unitNum, 'null')", "?unit");

            String query = sb.build().toString();
            JSONArray queryResultArray = storeClient.executeQuery(query);

            List<OSMBuilding> osmBuildings = new ArrayList<>();

            for (int i = 0; i < queryResultArray.length(); i++) {
                OSMBuilding osmBuilding = new OSMBuilding();
                String[] buildingIri = queryResultArray.getJSONObject(i).getString("building").split("Building/");
                osmBuilding.setBuildingIri(buildingIri[1]);
                osmBuilding.setStreet(queryResultArray.getJSONObject(i).getString("street"));
                osmBuilding.setUsage(queryResultArray.getJSONObject(i).getString("usage"));
                osmBuilding.setPostcode(queryResultArray.getJSONObject(i).getString("postcode"));
                osmBuilding.seUnit(queryResultArray.getJSONObject(i).getString("unit"));
                osmBuildings.add(osmBuilding);
            }
            return osmBuildings;
        }catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
    }

}
