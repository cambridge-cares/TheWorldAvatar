package uk.ac.cam.cares.jsp.integration;

import com.intuit.fuzzymatcher.component.MatchService;
import com.intuit.fuzzymatcher.domain.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class BuildingLink {
    List<GeoObject3D> geoObject3Ds = new ArrayList<>();
    List<KGObjects> kgObjects = new ArrayList<>();

    BuildingLink () {}
    BuildingLink (List<GeoObject3D> geoObject3Ds, List<KGObjects> kgObjects){
        this.geoObject3Ds = geoObject3Ds;
        this.kgObjects = kgObjects;
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
