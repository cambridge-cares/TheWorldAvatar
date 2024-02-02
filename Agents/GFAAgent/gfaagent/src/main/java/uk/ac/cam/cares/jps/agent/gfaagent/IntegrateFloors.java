package uk.ac.cam.cares.jps.agent.gfaagent;

import java.io.FileReader;
import java.io.IOException;
import java.util.Map;
import java.util.List;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import com.opencsv.bean.CsvToBeanBuilder;

public class IntegrateFloors {

    private final String dbUrl;
    private final String user;
    private final String password;

    private RemoteRDBStoreClient postgisClient;
    
    public IntegrateFloors (String floorsCsv, String postgisDb, String postgisUser, String postgisPassword) throws IOException{
        this.dbUrl = postgisDb;
        this.user = postgisUser;
        this.password = postgisPassword;
        this.postgisClient = new RemoteRDBStoreClient(dbUrl, user, password);

        List<FloorsCsv> hdbFloors = new CsvToBeanBuilder(new FileReader(floorsCsv))
                .withType(FloorsCsv.class)
                .build()
                .parse();
            

        // beans.forEach(System.out::println);
    }
}
