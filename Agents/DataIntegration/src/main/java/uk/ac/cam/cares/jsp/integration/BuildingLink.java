package uk.ac.cam.cares.jsp.integration;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
@WebServlet(urlPatterns = {"/BuildingLink"})
public class BuildingLink extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(SpatialLink.class);
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

        String db3d;
        GeoObject3D object3D = new GeoObject3D();
        String kgurl = req.getParameter("iri");
        RemoteStoreClient kgClient = new RemoteStoreClient(kgurl,kgurl,null,null);
        KGObjects kgObjects = new KGObjects(kgClient, null, null, null, null);
        String type = req.getParameter("type"); //building type (related to namespace)
        try {
            this.kgObjects =  kgObjects.getAllObjects(type);

            db3d = req.getParameter("db3d");

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
        List<Document> docNameList = new ArrayList<>();
        List<Document> docAddressList = new ArrayList<>();
        for (int j = 0; j < geoObject3Ds.size(); j++){
            if(geoObject3Ds.get(j).getName() != null){
                Document preDocument = new Document.Builder(geoObject3Ds.get(j).getGmlId())
                        .addElement(new Element.Builder<String>().setValue(geoObject3Ds.get(j).getName()).setType(ElementType.NAME).createElement())
                        .createDocument();
                docNameList.add(preDocument);
            }
            if(geoObject3Ds.get(j).getAddress() != null){
                String address = geoObject3Ds.get(j).getAddress().getHouse() + "," + geoObject3Ds.get(j).getAddress().getStreet() + "," + geoObject3Ds.get(j).getAddress().getZipCode();
                Document preDocument = new Document.Builder(geoObject3Ds.get(j).getGmlId())
                        .addElement(new Element.Builder<String>().setValue(address).setType(ElementType.ADDRESS).createElement())
                        .createDocument();
                docAddressList.add(preDocument);
            }
        }

        for(int i = 0; i < kgObjects.size(); i++){
            if(kgObjects.get(i).getObjectName() != null){
                double score = 0.0;
                Document matchNameDoc = new Document.Builder(kgObjects.get(i).getObjectIri())
                        .addElement(new Element.Builder<String>().setValue(kgObjects.get(i).getObjectName()).setType(ElementType.NAME).createElement())
                        .createDocument();

                String kgAddress = kgObjects.get(i).getAddress().getStreetNum() + kgObjects.get(i).getAddress().getStreet() + kgObjects.get(i).getAddress().getPostalcode();
                Document matchAddressDoc = new Document.Builder(kgObjects.get(i).getObjectIri())
                        .addElement(new Element.Builder<String>().setValue(kgAddress).setType(ElementType.ADDRESS).createElement())
                        .createDocument();

                Map<String, List<Match<Document>>> resultName = matchService.applyMatchByDocId(matchNameDoc,docNameList);
                Map<String, List<Match<Document>>> resultAddress = matchService.applyMatchByDocId(matchAddressDoc,docAddressList);

                for (Map.Entry<String, List<Match<Document>>> entry : resultName.entrySet()) {
                    for (Match<Document> match : entry.getValue()) {
                        if(match.getScore().getResult()>score && match.getScore().getResult()>0.5){
                            System.out.println("Data: " + match.getData() + " Matched With: " + match.getMatchedWith() + " Score: " + match.getScore().getResult());
                            score = match.getScore().getResult();
                            kgObjects.get(i).setObjectId(match.getMatchedWith().getKey());
                        }
                    }
                }

                for (Map.Entry<String, List<Match<Document>>> entry : resultAddress.entrySet()) {
                    for (Match<Document> match : entry.getValue()) {
                        if(match.getScore().getResult()>score && match.getScore().getResult()>0.5){
                            System.out.println("Data: " + match.getData() + " Matched With: " + match.getMatchedWith() + " Score: " + match.getScore().getResult());
                            score = match.getScore().getResult();
                            if(kgObjects.get(i).getGeoObjectId() == null){
                                kgObjects.get(i).setObjectId(match.getMatchedWith().getKey());
                            }
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
}
