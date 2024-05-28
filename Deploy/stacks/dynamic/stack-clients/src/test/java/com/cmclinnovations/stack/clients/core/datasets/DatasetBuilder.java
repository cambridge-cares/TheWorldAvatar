package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

import com.cmclinnovations.stack.clients.blazegraph.Namespace;
import com.cmclinnovations.stack.clients.geoserver.GeoServerStyle;
import com.cmclinnovations.stack.clients.geoserver.StaticGeoServerData;

class DatasetBuilder {

    private String name;

    private String description;

    private Path datasetDirectory;

    private String database;

    private Namespace namespace;

    private String workspaceName;

    private List<String> externalDatasetNames;

    private List<DataSubset> dataSubsets;

    private List<GeoServerStyle> geoserverStyles;

    private List<String> ontopMappings;

    private StaticGeoServerData staticGeoServerData;

    private String rdfType;

    private String baseIRI;

    DatasetBuilder(String name) {
        this.name = name;
    }

    public DatasetBuilder withDescription(String description) {
        this.description = description;
        return this;
    }

    public DatasetBuilder withDatasetDirectory(String datasetDirectory) {
        this.datasetDirectory = Path.of(datasetDirectory);
        return this;
    }

    public DatasetBuilder withDatabase(String database) {
        this.database = database;
        return this;
    }

    public DatasetBuilder withNamespace(Namespace namespace) {
        this.namespace = namespace;
        return this;
    }

    public DatasetBuilder withWorkspaceName(String workspaceName) {
        this.workspaceName = workspaceName;
        return this;
    }

    public DatasetBuilder withExternalDatasetNames(List<String> externalDatasetNames) {
        this.externalDatasetNames = externalDatasetNames;
        return this;
    }

    public DatasetBuilder withDataSubsets(List<DataSubset> dataSubsets) {
        this.dataSubsets = dataSubsets;
        return this;
    }

    public DatasetBuilder withGeoserverStyles(List<GeoServerStyle> geoserverStyles) {
        this.geoserverStyles = geoserverStyles;
        return this;
    }

    public DatasetBuilder withOntopMappings(List<String> ontopMappings) {
        this.ontopMappings = ontopMappings;
        return this;
    }

    public DatasetBuilder withStaticGeoServerData(StaticGeoServerData staticGeoServerData) {
        this.staticGeoServerData = staticGeoServerData;
        return this;
    }

    public DatasetBuilder withRdfType(String rdfType) {
        this.rdfType = rdfType;
        return this;
    }

    public DatasetBuilder withBaseIRI(String baseIRI) {
        this.baseIRI = baseIRI;
        return this;
    }

    Dataset build() {
        return new Dataset(name, Optional.ofNullable(description), Optional.ofNullable(datasetDirectory),
                Optional.ofNullable(database), Optional.ofNullable(namespace), Optional.ofNullable(workspaceName),
                Optional.ofNullable(externalDatasetNames), Optional.ofNullable(dataSubsets),
                Optional.ofNullable(geoserverStyles), Optional.ofNullable(staticGeoServerData),
                Optional.ofNullable(ontopMappings), false, Optional.ofNullable(rdfType),
                Optional.ofNullable(baseIRI));
    }
}