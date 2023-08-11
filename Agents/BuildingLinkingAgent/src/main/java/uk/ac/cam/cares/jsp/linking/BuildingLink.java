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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class BuildingLink{
    private static final Logger LOGGER = LogManager.getLogger(BuildingLink.class);
    private static final String INVALID_CONNECTION_MESSAGE = "Connection is invalid...";
    private SqlConnectionPool pool;
//    private static final File configPath = File.of("/inputs/config");
    List<GeoObject3D> geoObject3Ds = new ArrayList<>();
    List<KGObjects> kgObjects = new ArrayList<>();

    // private PostgresClient postgisClient;

    // BuildingLink () {}
    // BuildingLink (List<GeoObject3D> geoObject3Ds, List<KGObjects> kgObjects){
    //     this.geoObject3Ds = geoObject3Ds;
    //     this.kgObjects = kgObjects;
    // }

    protected void BuildingLink(String[] config) {
//        
        GeoObject3D object3D = new GeoObject3D();

        try {
            String buildingIri = null;
            String objIRI = null;
            this.geoObject3Ds = object3D.getObject3D(config);
            RemoteStoreClient kgClient = new RemoteStoreClient(config[4],config[4],null,null);
            KGObjects obj = new KGObjects(kgClient, null, null, null, null);
            for(int i = 0; i < this.geoObject3Ds.size(); i++){
                buildingIri = this.geoObject3Ds.get(i).getIRI();
                objIRI = obj.queryObjectIri(buildingIri);
                if(objIRI == null){
                //generate a IRI for bot:building
                    String[] splitIRI = buildingIri.split("kg/",2);
                    String[] subSplitIRI = splitIRI[1].split("/");
                    UUID uuid = UUID.randomUUID();
                    objIRI = splitIRI[0] + "kg/" + subSplitIRI[0] + "/" + subSplitIRI[0] + "_" + uuid.toString();
                    obj.insertIri(objIRI, buildingIri);
                }else{
                //building link
                }
            }
        } catch (Exception e) {
            LOGGER.error("Fail to connect database.");
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
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

                Document preDocument = new Document.Builder(geoObject3Ds.get(j).getIRI())
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
                            kgObjects.get(i).setObjectIri(match.getMatchedWith().getKey());
                        }
                    }
                }
                kgObjects.get(i).updateOntoCityGML();
            }
        }


    }

}
