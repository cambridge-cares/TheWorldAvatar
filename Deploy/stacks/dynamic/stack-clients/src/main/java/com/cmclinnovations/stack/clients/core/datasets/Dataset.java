package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerStyle;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Dataset {

    private String name;
    private Path datasetDirectory;

    private final String database;
    private final String namespace;
    private final String workspaceName;

    private List<String> externalDatasets;

    private List<DataSubset> dataSubsets;

    private List<GeoServerStyle> geoserverStyles;

    private List<String> ontopMappings;

    private boolean skip;

    @JsonCreator
    public Dataset(@JsonProperty(value = "name") String name,
            @JsonProperty(value = "datasetDirectory") Path datasetDirectory,
            @JsonProperty(value = "database") String database,
            @JsonProperty(value = "namespace") String namespace,
            @JsonProperty(value = "workspace") String workspaceName,
            @JsonProperty(value = "externalDatasets") List<String> externalDatasets,
            @JsonProperty(value = "dataSubsets") List<DataSubset> dataSubsets,
            @JsonProperty(value = "styles") List<GeoServerStyle> geoserverStyles,
            @JsonProperty(value = "mappings") List<String> ontopMappings,
            @JsonProperty(value = "skip") boolean skip) {
        this.name = name;
        this.datasetDirectory = (null != datasetDirectory) ? datasetDirectory : Path.of(name);
        this.database = database;
        this.namespace = namespace;
        this.workspaceName = (null != workspaceName) ? workspaceName : name;
        this.externalDatasets = (null != externalDatasets) ? externalDatasets : new ArrayList<>();
        this.dataSubsets = (null != dataSubsets) ? dataSubsets : new ArrayList<>();
        this.geoserverStyles = (null != geoserverStyles) ? geoserverStyles : new ArrayList<>();
        this.ontopMappings = (null != ontopMappings) ? ontopMappings : new ArrayList<>();
        this.skip = skip;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Path getDirectory() {
        return Path.of("/inputs", "data").resolve(datasetDirectory);
    }

    public String getDatabase() {
        return database;
    }

    public String getNamespace() {
        return namespace;
    }

    public String getWorkspaceName() {
        return workspaceName;
    }

    public List<String> getExternalDatasets() {
        return externalDatasets;
    }

    public boolean isSkip() {
        return skip;
    }

    public void loadData() {
        if (!skip) {

            if (null != database) {
                PostGISClient.getInstance().createDatabase(database);
            }
            if (null != namespace) {
                BlazegraphClient.getInstance().createNamespace(namespace);
            }
            if (null != workspaceName) {
                GeoServerClient geoServerClient = GeoServerClient.getInstance();
                geoServerClient.createWorkspace(workspaceName);
                geoserverStyles.forEach(style -> geoServerClient.loadStyle(style, workspaceName));
            }

            dataSubsets.forEach(subset -> subset.load(this));

            ontopMappings.forEach(mapping -> OntopClient.getInstance().updateOBDA(getDirectory().resolve(mapping)));
        }
    }

}
