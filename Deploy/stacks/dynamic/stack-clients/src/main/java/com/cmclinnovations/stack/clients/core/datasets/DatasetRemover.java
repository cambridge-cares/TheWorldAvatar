package com.cmclinnovations.stack.clients.core.datasets;

import java.util.Collection;
import java.util.stream.Stream;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.services.ServiceManager;

public class DatasetRemover {

    private static final ServiceManager serviceManager = new ServiceManager(false);

    private final String catalogNamespace;

    public DatasetRemover(String catalogNamespace) {
        this.catalogNamespace = catalogNamespace;
    }

    public DatasetRemover() {
        this("kb");
    }

    public void removeDatasets(Collection<Dataset> selectedDatasets) {
        selectedDatasets.forEach(this::removeDataset);
    }

    public void removeDatasets(Stream<Dataset> selectedDatasets) {
        selectedDatasets.forEach(this::removeDataset);
    }

    public void removeDataset(Dataset dataset) {

        if (!dataset.isSkip()) {

            String ontopServiceName = dataset.getOntopName();

            serviceManager.removeService(StackClient.getStackName(), ontopServiceName);

            GeoServerClient geoServerClient = GeoServerClient.getInstance();
            String workspaceName = dataset.getWorkspaceName();
            // Ensure GeoServer workspace is removed
            geoServerClient.removeWorkspace(workspaceName);

            // Ensure Blazegraph namespace is removed, if specified
            BlazegraphClient.getInstance().removeNamespace(dataset.getNamespace());

            // Ensure PostGIS database is removed, if specified
            PostGISClient.getInstance().removeDatabase(dataset.getDatabase());

            // Upload styles to GeoServer
            // dataset.getGeoserverStyles().forEach(style ->
            // geoServerClient.removeStyle(style, workspaceName));

            // if (dataset.hasStaticGeoServerData()) {
            // GeoServerClient geoServerClient = GeoServerClient.getInstance();
            // StaticGeoServerData staticGeoServerData = dataset.getStaticGeoServerData();
            // geoServerClient.loadIcons(directory, staticGeoServerData.getIconsDir());
            // geoServerClient.loadOtherFiles(directory,
            // staticGeoServerData.getOtherFiles());
            // }

            // record added datasets in the default kb namespace
            BlazegraphClient.getInstance().getRemoteStoreClient(catalogNamespace)
                    .executeUpdate(new DCATUpdateQuery().getDeleteQuery(dataset));
        }
    }

}
