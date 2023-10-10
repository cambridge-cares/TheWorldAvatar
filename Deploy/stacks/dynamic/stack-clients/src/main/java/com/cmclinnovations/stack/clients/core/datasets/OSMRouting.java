package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.core.Options;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.postgis.PGRoutingClient;
import com.fasterxml.jackson.annotation.JsonProperty;

import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.GSVirtualTableEncoder;

public class OSMRouting extends GeoServerDataSubset {
    @JsonProperty
    protected Options osm2PGRoutingOptions = new Options();

    @JsonProperty
    protected GeoServerVectorSettings waysGeoServerSettings = new GeoServerVectorSettings();
    @JsonProperty
    protected GeoServerVectorSettings verticesGeoServerSettings = new GeoServerVectorSettings();
    @JsonProperty
    protected GeoServerVectorSettings poiGeoServerSettings = new GeoServerVectorSettings();

    @Override
    public void loadData(Path dirPath, String database, String baseIRI) {
        PGRoutingClient.getInstance().uploadRoutingDataDirectoryToPostGIS(database, dirPath.toString(),
                getTablePrefix(), osm2PGRoutingOptions, false);
    }

    @Override
    public void createLayers(String workspaceName, String database) {
        createLayer(workspaceName, database, prefixTableName("ways"), waysGeoServerSettings);
        createLayer(workspaceName, database, prefixTableName("ways_vertices_pgr"), verticesGeoServerSettings);
        createLayer(workspaceName, database, prefixTableName("pointsofinterest"), poiGeoServerSettings);
    }

    private void createLayer(String workspaceName, String database, String layerName,
            GeoServerVectorSettings geoServerVectorSettings) {
        GSVirtualTableEncoder virtualTable = geoServerVectorSettings.getVirtualTable();
        if (null != virtualTable) {
            virtualTable.setSql(handleFileValues(virtualTable.getSql()));
        }

        GeoServerClient.getInstance()
                .createPostGISLayer(workspaceName, database, layerName, geoServerVectorSettings);
    }

    private String prefixTableName(String tableName) {
        return getTablePrefix() + tableName;
    }

    private String getTablePrefix() {
        return getName() + "_";
    }
}
