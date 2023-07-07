package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import com.cmclinnovations.stack.clients.blazegraph.Namespace;
import com.cmclinnovations.stack.clients.core.visualisationsources.VisSource;
import com.cmclinnovations.stack.clients.geoserver.GeoServerStyle;
import com.cmclinnovations.stack.clients.geoserver.StaticGeoServerData;
import com.fasterxml.jackson.annotation.JacksonInject;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Dataset {

    public static final String NAME_KEY = "name";

    private String name;

    private final Path datasetDirectory;

    private final String database;
    private final Namespace namespace;
    private final String workspaceName;

    private final List<String> externalDatasets;
    private final List<DataSubset> dataSubsets;
    private final List<VisSource> visualisationSources;
    private final List<GeoServerStyle> geoserverStyles;
    private final List<String> ontopMappings;
    private final StaticGeoServerData staticGeoServerData;

    private final boolean skip;

    @JsonCreator
    public Dataset(@JsonProperty(value = NAME_KEY) @JacksonInject(NAME_KEY) String name,
            @JsonProperty(value = "datasetDirectory") Path datasetDirectory,
            @JsonProperty(value = "database") String database,
            @JsonProperty(value = "namespace") Namespace namespace,
            @JsonProperty(value = "workspace") String workspaceName,
            @JsonProperty(value = "externalDatasets") List<String> externalDatasets,
            @JsonProperty(value = "dataSubsets") List<DataSubset> dataSubsets,
            @JsonProperty(value = "visualisationSources") List<VisSource> visualisationSources,
            @JsonProperty(value = "styles") List<GeoServerStyle> geoserverStyles,
            @JsonProperty(value = "mappings") List<String> ontopMappings,
            @JsonProperty(value = "staticGeoServerData") StaticGeoServerData staticGeoServerData,
            @JsonProperty(value = "skip") boolean skip) {
        this.name = name;
        this.datasetDirectory = datasetDirectory;
        this.database = database;
        this.namespace = namespace;
        this.workspaceName = workspaceName;
        this.externalDatasets = externalDatasets;
        this.dataSubsets = dataSubsets;
        this.visualisationSources = visualisationSources;
        this.geoserverStyles = geoserverStyles;
        this.ontopMappings = ontopMappings;
        this.staticGeoServerData = staticGeoServerData;
        this.skip = skip;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Path getDirectory() {
        return Path.of("/inputs", "data").resolve((null != datasetDirectory) ? datasetDirectory : Path.of(name));
    }

    public String getDatabase() {
        return (null != database) ? database : "postgres";
    }

    public String getNamespace() {
        if (null != namespace && null != namespace.getName()) {
            return namespace.getName();
        } else {
            return name;
        }
    }

    public Properties getNamespaceProperties() {
        if (null != namespace && null != namespace.getProperties()) {
            return namespace.getProperties();
        } else {
            return new Properties();
        }
    }

    public String getWorkspaceName() {
        return (null != workspaceName) ? workspaceName : name;
    }

    public List<String> getExternalDatasets() {
        return (null != externalDatasets) ? externalDatasets : Collections.emptyList();
    }

    public List<DataSubset> getDataSubsets() {
        return (null != dataSubsets) ? dataSubsets : Collections.emptyList();
    }

    public List<VisSource> getVisualisationSources() {
        return (null != visualisationSources) ? visualisationSources : Collections.emptyList();
    }

    public List<GeoServerStyle> getGeoserverStyles() {
        return (null != geoserverStyles) ? geoserverStyles : Collections.emptyList();
    }

    public List<String> getOntopMappings() {
        return (null != ontopMappings) ? ontopMappings : Collections.emptyList();
    }

    public StaticGeoServerData getStaticGeoServerData() {
        return staticGeoServerData;
    }

    public boolean isSkip() {
        return skip;
    }

}
