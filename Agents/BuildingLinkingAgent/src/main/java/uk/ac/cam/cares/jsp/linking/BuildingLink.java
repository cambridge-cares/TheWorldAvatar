package uk.ac.cam.cares.jsp.linking;
/**
 * Link geospatial object (3D model) and object in TWA
 * PostgreSQL: 3D model; Blazegraph: building information data
 * Parameter: PostgreSQL database name, blazegraph queryEndpoint and updateEndpoint
 * @author Jingya yan
 *
 */

import com.intuit.fuzzymatcher.component.MatchService;
import com.intuit.fuzzymatcher.domain.Document;
import com.intuit.fuzzymatcher.domain.Element;
import com.intuit.fuzzymatcher.domain.ElementType;
import com.intuit.fuzzymatcher.domain.Match;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

@WebServlet(urlPatterns = {"/BuildingLink"})
public class BuildingLink extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(BuildingLink.class);
//    private static final File configPath = File.of("/inputs/config");
    private static String file ="/inputs/config/buildinglink";
    List<GeoObject3D> geoObject3Ds = new ArrayList<>();
    List<KGObjects> kgObjects = new ArrayList<>();

    private PostgresClient postgisClient;

    BuildingLink () {}
    BuildingLink (List<GeoObject3D> geoObject3Ds, List<KGObjects> kgObjects){
        this.geoObject3Ds = geoObject3Ds;
        this.kgObjects = kgObjects;
    }
    protected void doPut(HttpServletRequest req, HttpServletResponse resp) {
        new Config().initProperties();
        LOGGER.info("Received POST request to link building");
        LOGGER.info("Received request: " + req);

        Map<String, String> parameters = aggregateByKeys();
        String db3d = parameters.get("db3d");
        String kgurl = parameters.get("blazegraph");

        GeoObject3D object3D = new GeoObject3D();

        RemoteStoreClient kgClient = new RemoteStoreClient(kgurl,kgurl,null,null);
        KGObjects kgObjects = new KGObjects(kgClient, null, null, null, null);
//        String type = req.getParameter("type"); //building type (related to namespace)
        try {
            this.kgObjects =  kgObjects.getAllObjects(parameters);

//            db3d = req.getParameter("db3d");

            if (postgisClient == null) {
                postgisClient = new PostgresClient(Config.dburl + "/" + db3d, Config.dbuser, Config.dbpassword);
            }
//            PostgresClient conn3 = new PostgresClient(Config.dburl + "/" + db3d, Config.dbuser, Config.dbpassword);
            object3D.setPostGISClient(postgisClient);
            this.geoObject3Ds = object3D.getObject3D();
        } catch (Exception e) {
            LOGGER.error("Fail to connect database.");
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
        fuzzyMatch(this.geoObject3Ds,this.kgObjects);

    }
    public void fuzzyMatch(List<GeoObject3D> geoObject3Ds, List<KGObjects> kgObjects){
        MatchService matchService = new MatchService();
        List<Document> docmentList = new ArrayList<>();
        for (int j = 0; j < geoObject3Ds.size(); j++){
            if(geoObject3Ds.get(j).getName() != null || geoObject3Ds.get(j).getAddress().getGmlId() != null){
                String name = geoObject3Ds.get(j).getName();
                if (name == null) {
                    name = "NULL";
                }
                String address = geoObject3Ds.get(j).getAddress().getHouse() + " " + geoObject3Ds.get(j).getAddress().getStreet() + " " + geoObject3Ds.get(j).getAddress().getZipCode();

                Document preDocument = new Document.Builder(geoObject3Ds.get(j).getGmlId())
                        .addElement(new Element.Builder<String>().setValue(name).setType(ElementType.NAME).setWeight(0.5).createElement())
                        .addElement(new Element.Builder<String>().setValue(address).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                        .createDocument();
                docmentList.add(preDocument);
            }
        }
        for(int i = 0; i < kgObjects.size(); i++){
            if(kgObjects.get(i).getObjectName() != null || kgObjects.get(i).getAddress() != null){
                double score = 0.0;
                String kgName = kgObjects.get(i).getObjectName();
                if (kgName == null) {
                    kgName = "BLANK";
                }
                String kgAddress = kgObjects.get(i).getAddress().getStreetNum() + " " + kgObjects.get(i).getAddress().getStreet() + " " + kgObjects.get(i).getAddress().getPostalcode();
                Document matchDoc = new Document.Builder(kgObjects.get(i).getObjectIri())
                        .addElement(new Element.Builder<String>().setValue(kgName).setType(ElementType.NAME).setWeight(0.5).createElement())
                        .addElement(new Element.Builder<String>().setValue(kgAddress).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                        .setThreshold(0.2).createDocument();

                Map<String, List<Match<Document>>> resultName = matchService.applyMatchByDocId(matchDoc,docmentList);

                for (Map.Entry<String, List<Match<Document>>> entry : resultName.entrySet()) {
                    for (Match<Document> match : entry.getValue()) {
                        if(match.getScore().getResult()>score && match.getScore().getResult()>0.2){
                            System.out.println("Data: " + match.getData() + " Matched With: " + match.getMatchedWith() + " Score: " + match.getScore().getResult());
                            score = match.getScore().getResult();
                            kgObjects.get(i).setObjectId(match.getMatchedWith().getKey());
                        }
                    }
                }
                kgObjects.get(i).updateOntoCityGML();
            }
        }


    }

    void setPostGISClient(PostgresClient postgisClient) {
        this.postgisClient = postgisClient;
    }

    public static Map<String, String> aggregateByKeys() {
        Map<String, String> map = new HashMap<>();
        try (Stream<String> lines = Files.lines(Paths.get(file))) {
            lines.filter(line -> line.contains(":"))
                    .forEach(line -> {
                        String[] keyValuePair = line.split(":", 2);
                        String key = keyValuePair[0];
                        String value = keyValuePair[1];
                        map.put(key, value);

                    });
        } catch (IOException e) {
            e.printStackTrace();
        }
        return map;
    }
}
