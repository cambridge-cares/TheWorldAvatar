package uk.ac.cam.cares.jps.agent.gfaagent;

import java.io.FileReader;
import java.io.IOException;
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

    public void matchAddress (String floorsCsv) throws IOException{
        MatchService matchService = new MatchService();
        //query address infor from osm db
        List<Document> polyDoc = new ArrayList<>();
        JSONArray polyResults = postgisClient.executeQuery(polygonSQLQuery);
        for (int i = 0; i < polyResults.length(); i++){
            String osmAddress = polyResults.getJSONObject(i).getString("addr_street");
            String num = polyResults.getJSONObject(i).getString("addr_housenumber");
            int id = polyResults.getJSONObject(i).getInt("ogc_fid");
            Document preDocument = new Document.Builder(String.valueOf(id))
                .addElement(new Element.Builder<String>().setValue(num).setType(ElementType.NUMBER).setWeight(0.5).createElement())
                .addElement(new Element.Builder<String>().setValue(osmAddress).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                .createDocument();
                polyDoc.add(preDocument);
        }

        List<Document> pointDoc = new ArrayList<>();
        JSONArray pointResults = postgisClient.executeQuery(pointSQLQuery);
        for (int i = 0; i < polyResults.length(); i++){
            String osmAddress = pointResults.getJSONObject(i).getString("addr_street");
            String num = pointResults.getJSONObject(i).getString("addr_housenumber");
            int id = pointResults.getJSONObject(i).getInt("ogc_fid");
            Document preDocument = new Document.Builder(String.valueOf(id))
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
            String polyId = null;
            for (Map.Entry<String, List<Match<Document>>> entry : resultPoly.entrySet()) {
                for (Match<Document> match : entry.getValue()) {
                    if(match.getScore().getResult()>polyScore && match.getScore().getResult()>0.5){
                        System.out.println("Data: " + match.getData() + " Matched With: " + match.getMatchedWith() + " Score: " + match.getScore().getResult());
                        polyScore = match.getScore().getResult();
                        polyId = match.getMatchedWith().getKey();
                    }
                }
            }
            String pointId = null;
            for (Map.Entry<String, List<Match<Document>>> entry : resultPoint.entrySet()) {
                for (Match<Document> match : entry.getValue()) {
                    if(match.getScore().getResult()>pointScore && match.getScore().getResult()>0.5){
                        System.out.println("Data: " + match.getData() + " Matched With: " + match.getMatchedWith() + " Score: " + match.getScore().getResult());
                        pointScore = match.getScore().getResult();
                        pointId = match.getMatchedWith().getKey();
                    }
                }
            }
            
            if(pointScore > polyScore && pointScore != 0) {

            } else if (polyScore != 0){

            }
        }
        
        // beans.forEach(System.out::println);
    }

    private static final String polygonSQLQuery = "SELECT ogc_fid, addr_street, addr_housenumber, building_levels, building_levels_underground FROM polygons";
    private static final String pointSQLQuery = "SELECT ogc_fid, addr_street, addr_housenumber, building_levels, building_levels_underground FROM points";

}
