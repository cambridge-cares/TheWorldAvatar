package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.postgis.PostGISClient;

public class OSMRouting extends Vector {

    @Override
    public void loadData(Path dirPath, String database) {
        PostGISClient.getInstance().uploadRoutingDataDirectoryToPostGIS(database, dirPath.toString());
    }
}
