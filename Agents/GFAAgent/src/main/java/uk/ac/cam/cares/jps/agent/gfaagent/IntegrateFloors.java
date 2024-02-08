package uk.ac.cam.cares.jps.agent.gfaagent;

import java.io.FileReader;
import java.io.IOException;
import java.util.Map;
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
        List<Document> docmentList = new ArrayList<>();

        List<FloorsCsv> hdbFloors = new CsvToBeanBuilder(new FileReader(floorsCsv))
                .withType(FloorsCsv.class)
                .build()
                .parse();
        
        
       
        for (int i = 0; i < hdbFloors.size(); i++){
            String num =  hdbFloors.get(i).getBLK();
            String address = hdbFloors.get(i).getStreet();

            Document preDocument = new Document.Builder(String.valueOf(i))
                    .addElement(new Element.Builder<String>().setValue(num).setType(ElementType.NAME).setWeight(0.5).createElement())
                    .addElement(new Element.Builder<String>().setValue(address).setType(ElementType.ADDRESS).setWeight(0.5).createElement())
                    .createDocument();
            docmentList.add(preDocument);
        }
        // beans.forEach(System.out::println);
    }

    private static final String gfaSQLInsert = "SELECT addr_street, addr_housenumber, building_levels, building_levels_underground FROM polygons";

}
