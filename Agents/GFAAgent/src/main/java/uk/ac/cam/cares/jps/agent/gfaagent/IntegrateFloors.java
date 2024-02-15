package uk.ac.cam.cares.jps.agent.gfaagent;

import java.io.FileReader;
import java.io.IOException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Map;

import org.json.JSONArray;

import java.util.List;
import java.util.ArrayList;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import com.intuit.fuzzymatcher.component.MatchService;
import com.intuit.fuzzymatcher.domain.Document;
import com.intuit.fuzzymatcher.domain.Element;
import com.intuit.fuzzymatcher.domain.ElementType;
import com.intuit.fuzzymatcher.domain.Match;

import com.opencsv.bean.CsvToBeanBuilder;

public class IntegrateFloors {

    private final String dbUrl;
    private final String user;
    private final String password;

    private RemoteRDBStoreClient postgisClient;
    
    public IntegrateFloors (String postgisDb, String postgisUser, String postgisPassword){
        this.dbUrl = postgisDb;
        this.user = postgisUser;
        this.password = postgisPassword;
        this.postgisClient = new RemoteRDBStoreClient(dbUrl, user, password);

        
    }

    public void matchAddress (String floorsCsv) throws IOException{
        MatchService matchService = new MatchService();
        //query address infor from osm db
        List<Document> polyDoc = new ArrayList<>();
        List<Document> pointDoc = new ArrayList<>();
        try (Connection srcConn = postgisClient.getConnection()) {
            try (Statement stmt = srcConn.createStatement()) {
                ResultSet polyResults = stmt.executeQuery(polygonSQLQuery);
                while (polyResults.next()) {
                    String osmAddress = polyResults.getString("addr_street");
                    if (osmAddress == null){
                        osmAddress = "NULL";
                    }
                    String num = polyResults.getString("addr_housenumber");
                    if (num == null) {
                        num = "0";
                    }
                    String buildingiri = polyResults.getString("building_iri");
                    if (buildingiri != null) {
                        Document preDocument = new Document.Builder(buildingiri)
                        .addElement(new Element.Builder<String>().setValue(num).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                        .addElement(new Element.Builder<String>().setValue(osmAddress).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                        .createDocument();
                    polyDoc.add(preDocument);
                    }                   
                }
                               
                ResultSet pointResults = stmt.executeQuery(pointSQLQuery);
                while (pointResults.next()) {
                    String osmAddress = pointResults.getString("addr_street");
                    if (osmAddress == null){
                        osmAddress = "NULL";
                    }
                    String num = pointResults.getString("addr_housenumber");
                    if (num == null) {
                        num = "0";
                    }
                    String buildingiri = pointResults.getString("building_iri");
                    if (buildingiri != null) {
                        Document preDocument = new Document.Builder(buildingiri)
                        .addElement(new Element.Builder<String>().setValue(num).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                        .addElement(new Element.Builder<String>().setValue(osmAddress).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                        .createDocument();
                        pointDoc.add(preDocument);
                    }
                    
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
                    
                    Map<String, List<Match<Document>>> resultPoint = matchService.applyMatchByDocId(matchDoc,pointDoc);
                    Map<String, List<Match<Document>>> resultPoly = matchService.applyMatchByDocId(matchDoc,polyDoc);
                    
                    String polyIri = null;
                    for (Map.Entry<String, List<Match<Document>>> entry : resultPoly.entrySet()) {
                        for (Match<Document> match : entry.getValue()) {
                            if(match.getScore().getResult()>polyScore && match.getScore().getResult()>0.5){
                                System.out.println("Data: " + match.getData() + " Matched With: " + match.getMatchedWith() + " Score: " + match.getScore().getResult());
                                polyScore = match.getScore().getResult();
                                polyIri = match.getMatchedWith().getKey();
                            }
                        }
                    }
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
                    int floors = hdbFloors.get(i).getFloors();
                    String buildingiri = null;
                    if(pointScore > polyScore && pointScore != 0) {
                        buildingiri = pointIri;
                    } else if (polyScore != 0){
                        buildingiri = polyIri;
                    }

                    if (buildingiri != null) {
                        String buildingSQLUpdate = "UPDATE building b SET storeys_above_ground = " + floors + 
                                                " FROM cityobject_genericattrib cg\n" + 
                                                "WHERE b.id = cg.cityobject_id AND cg.strval = '" + buildingiri + "';";
                        postgisClient.executeUpdate(buildingSQLUpdate);
                    }
                    
                }
            }
        } catch (SQLException e) {
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }       
    }

    private static final String polygonSQLQuery = "SELECT ogc_fid, addr_street, addr_housenumber, building_levels, building_iri FROM polygons WHERE addr_street IS NOT NULL OR addr_housenumber IS NOT NULL";
    private static final String pointSQLQuery = "SELECT ogc_fid, addr_street, addr_housenumber, building_levels, building_iri FROM points WHERE addr_street IS NOT NULL OR addr_housenumber IS NOT NULL";

}
