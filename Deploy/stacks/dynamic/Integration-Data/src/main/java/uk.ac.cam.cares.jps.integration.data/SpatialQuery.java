package uk.ac.cam.cares.jps.integration.data;

import java.sql.SQLException;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;

public class SpatialQuery {

    public void getPostgres(String database){
        StackClient.getRemoteRDBStoreClient();

    }
}
