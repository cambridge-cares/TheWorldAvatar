package uk.ac.cam.cares.jps.agent.gfaagent;

import java.io.FileReader;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

import org.json.JSONArray;

import java.util.List;
import java.util.ArrayList;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

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

    public void matchAddress (String floorsCsv) throws IOException, SQLException{
        MatchService matchService = new MatchService();
        //query address infor from osm db
        List<Document> polyDoc = new ArrayList<>();
        ResultSet polyResults = postgisClient.executeQuerytoResultSet(polygonSQLQuery);
        while (polyResults.next()) {
            String osmAddress = polyResults.getString("addr_street");
            String num = polyResults.getString("addr_housenumber");
            int id = polyResults.getInt("ogc_fid");
            String buildingiri = polyResults.getString("building_iri");
            Document preDocument = new Document.Builder(buildingiri)
                .addElement(new Element.Builder<String>().setValue(num).setType(ElementType.NUMBER).setWeight(0.5).createElement())
                .addElement(new Element.Builder<String>().setValue(osmAddress).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                .createDocument();
            polyDoc.add(preDocument);
        }

        List<Document> pointDoc = new ArrayList<>();
        JSONArray pointResults = postgisClient.executeQuery(pointSQLQuery);
        for (int i = 0; i < pointResults.length(); i++){
            String osmAddress = pointResults.getJSONObject(i).getString("addr_street");
            String num = pointResults.getJSONObject(i).getString("addr_housenumber");
            // int id = pointResults.getJSONObject(i).getInt("ogc_fid");
            Document preDocument = new Document.Builder(String.valueOf(i))
                .addElement(new Element.Builder<String>().setValue(num).setType(ElementType.NUMBER).setWeight(0.5).createElement())
                .addElement(new Element.Builder<String>().setValue(osmAddress).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                .createDocument();
                pointDoc.add(preDocument);
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
                    .addElement(new Element.Builder<String>().setValue(blk).setType(ElementType.NUMBER).setWeight(0.5).createElement())
                    .addElement(new Element.Builder<String>().setValue(address).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                    .setThreshold(0.5).createDocument();
            
            Map<String, List<Match<Document>>> resultPoly = matchService.applyMatchByDocId(matchDoc,polyDoc);
            Map<String, List<Match<Document>>> resultPoint = matchService.applyMatchByDocId(matchDoc,pointDoc);
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
            int pointId = -1;
            for (Map.Entry<String, List<Match<Document>>> entry : resultPoint.entrySet()) {
                for (Match<Document> match : entry.getValue()) {
                    if(match.getScore().getResult()>pointScore && match.getScore().getResult()>0.5){
                        System.out.println("Data: " + match.getData() + " Matched With: " + match.getMatchedWith() + " Score: " + match.getScore().getResult());
                        pointScore = match.getScore().getResult();
                        pointId = Integer.valueOf(match.getMatchedWith().getKey());
                    }
                }
            }
            
            //store floors data based on building iri from osm agent
            int floors = hdbFloors.get(i).getFloors();
            String buildingiri = null;
            if(pointScore > polyScore && pointScore != 0) {
                buildingiri = pointResults.getJSONObject(pointId).getString("building_iri");
            } else if (polyScore != 0){
                buildingiri = polyIri;
            }

            String buildingSQLUpdate = "UPDATE b SET b.storeys_above_ground = " + floors + 
                                        "FROM building b\n" + 
                                        "INNER JOIN\n" + 
                                        "cityobject_genericattrib cg\n" + 
                                        "ON b.id = cg.cityobject_id AND cg.strval = " + buildingiri;
            postgisClient.executeUpdate(buildingSQLUpdate);
        }
        
        // beans.forEach(System.out::println);
    }

    private static final String polygonSQLQuery = "SELECT ogc_fid, addr_street, addr_housenumber, building_levels, building_levels_underground, building_iri FROM polygons WHERE addr_street IS NOT NULL OR addr_housenumber IS NOT NULL";
    private static final String pointSQLQuery = "SELECT ogc_fid, addr_street, addr_housenumber, building_levels, building_levels_underground, building_iri FROM points WHERE addr_street IS NOT NULL OR addr_housenumber IS NOT NULL";

}
