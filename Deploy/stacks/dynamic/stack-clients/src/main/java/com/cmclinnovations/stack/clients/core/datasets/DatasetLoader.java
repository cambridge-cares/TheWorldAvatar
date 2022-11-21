package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;
import java.util.List;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;

public class DatasetLoader {

    private DatasetLoader() {
    }

    public static void loadData(Dataset dataset) {
        if (!dataset.isSkip()) {
            List<DataSubset> dataSubsets = dataset.getDataSubsets();
            // Ensure PostGIS database exists, if specified
            if (dataSubsets.stream().anyMatch(DataSubset::usesPostGIS)) {
                PostGISClient.getInstance().createDatabase(dataset.getDatabase());
            }

            // Ensure Blazegraph namespace exists, if specified
            if (dataSubsets.stream().anyMatch(DataSubset::usesBlazegraph)) {
                BlazegraphClient.getInstance().createNamespace(dataset.getNamespace(),
                        dataset.getNamespaceProperties());
            }

            if (!dataset.getGeoserverStyles().isEmpty()
                    || dataSubsets.stream().anyMatch(DataSubset::usesGeoServer)) {
                GeoServerClient geoServerClient = GeoServerClient.getInstance();
                String workspaceName = dataset.getWorkspaceName();
                // Ensure GeoServer workspace exists
                geoServerClient.createWorkspace(workspaceName);
                // Upload styles to GeoServer
                dataset.getGeoserverStyles().forEach(style -> geoServerClient.loadStyle(style,
                        workspaceName));
            }

            dataSubsets.forEach(subset -> subset.load(dataset));

            OntopClient ontopClient = OntopClient.getInstance();
            Path directory = dataset.getDirectory();
            dataset.getOntopMappings().forEach(mapping -> ontopClient.updateOBDA(directory.resolve(mapping)));
        }
    }
}
