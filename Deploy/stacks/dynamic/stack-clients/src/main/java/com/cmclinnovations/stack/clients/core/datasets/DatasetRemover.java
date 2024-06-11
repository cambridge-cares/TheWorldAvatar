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

    private DatasetRemover() {
    }

    public static void removeDatasets(Collection<Dataset> selectedDatasets) {
        selectedDatasets.forEach(DatasetRemover::removeDataset);
    }

    public static void removeDatasets(Stream<Dataset> selectedDatasets) {
        selectedDatasets.forEach(DatasetRemover::removeDataset);
    }

    public static void removeDataset(Dataset dataset) {

        if (!dataset.isSkip()) {
            // Ensure PostGIS database is removed, if specified
            PostGISClient.getInstance().removeDatabase(dataset.getDatabase());

            // Ensure Blazegraph namespace is removed, if specified
            BlazegraphClient.getInstance().removeNamespace(dataset.getNamespace());

            GeoServerClient geoServerClient = GeoServerClient.getInstance();
            String workspaceName = dataset.getWorkspaceName();
            // Ensure GeoServer workspace is removed
            geoServerClient.removeWorkspace(workspaceName);

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

            String ontopServiceName = dataset.getOntopName();

            serviceManager.removeService(StackClient.getStackName(), ontopServiceName);

            // record added datasets in the default kb namespace
            BlazegraphClient.getInstance().getRemoteStoreClient("kb")
                    .executeUpdate(new DCATUpdateQuery().getDeleteQuery(dataset));
        }
    }

}
