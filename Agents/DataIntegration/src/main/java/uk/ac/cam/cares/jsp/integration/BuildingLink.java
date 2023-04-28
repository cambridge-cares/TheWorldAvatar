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
        RemoteStoreClient kgClient = new RemoteStoreClient(kgurl,kgurl,Config.kguser,Config.kgpassword);
        KGObjects kgObjects = new KGObjects(kgClient, null, null, null);

        try {
            this.kgObjects =  kgObjects.getAllObjects();

            db3d = req.getParameter("db3d");
            PostgresClient conn3 = new PostgresClient(Config.dburl + "/" + db3d, Config.dbuser, Config.dbpassword);
            object3D.setPostGISClient(conn3);
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
        List<Document> documentList = new ArrayList<>();
        for (int j = 0; j < geoObject3Ds.size(); j++){
            if(geoObject3Ds.get(j).getName() != null){
                Document preDocument = new Document.Builder(Integer.toString(geoObject3Ds.get(j).getId()))
                        .addElement(new Element.Builder<String>().setValue(geoObject3Ds.get(j).getName()).setType(ElementType.NAME).createElement())
                        .createDocument();
                documentList.add(preDocument);
            }
        }

        for(int i = 0; i < kgObjects.size(); i++){
            if(kgObjects.get(i).getObjectName() != null){
                double score = 0.0;
                Document matchDoc = new Document.Builder(kgObjects.get(i).getObjectIri())
                        .addElement(new Element.Builder<String>().setValue(kgObjects.get(i).getObjectName()).setType(ElementType.NAME).createElement())
                        .createDocument();

                Map<String, List<Match<Document>>> result = matchService.applyMatchByDocId(matchDoc,documentList);

                for (Map.Entry<String, List<Match<Document>>> entry : result.entrySet()) {
                    for (Match<Document> match : entry.getValue()) {
                        System.out.println("Data: " + match.getData() + " Matched With: " + match.getMatchedWith() + " Score: " + match.getScore().getResult());
                        if(match.getScore().getResult()>score && match.getScore().getResult()>0.5){
                            score = match.getScore().getResult();
                            kgObjects.get(i).setObjectId(match.getMatchedWith().getKey());
                        }
                    }
                }
                kgObjects.get(i).updateOntoCityGML();
            }
        }


    }
}
