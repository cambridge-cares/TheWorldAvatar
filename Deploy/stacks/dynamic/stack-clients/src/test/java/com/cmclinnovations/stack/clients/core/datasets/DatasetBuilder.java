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

    private List<String> rules;

    private List<String> ontologyDatasetNames;

    private StaticGeoServerData staticGeoServerData;

    private String rdfType;

    private String baseIRI;

    private Metadata metadataRDF;

    private List<Service> services = List.of();

    DatasetBuilder(String name) {
        this.name = name;
    }

    public DatasetBuilder withServices(Service... services) {
        this.services = List.of(services);
        return this;
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

    public DatasetBuilder withRules(List<String> rules) {
        this.rules = rules;
        return this;
    }

    public DatasetBuilder withOntologyDataset(List<String> ontologyDatasetNames) {
        this.ontologyDatasetNames = ontologyDatasetNames;
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

    public DatasetBuilder withMetadataRDF(Metadata metadataRDF) {
        this.metadataRDF = metadataRDF;
        return this;
    }

    Dataset build() {
        return new TestDataset(name, Optional.ofNullable(description), Optional.ofNullable(datasetDirectory),
                Optional.ofNullable(database), Optional.ofNullable(namespace), Optional.ofNullable(workspaceName),
                Optional.ofNullable(externalDatasetNames), Optional.ofNullable(dataSubsets),
                Optional.ofNullable(geoserverStyles), Optional.ofNullable(staticGeoServerData),
                Optional.ofNullable(ontopMappings), Optional.ofNullable(rules),
                Optional.ofNullable(ontologyDatasetNames), false, Optional.ofNullable(rdfType),
                Optional.ofNullable(baseIRI), Optional.ofNullable(metadataRDF), services);
    }

    static enum Service {
        NONE,
        BLAZEGRAPH,
        POSTGIS,
        GEOSERVER,
        ONTOP
    }

    static class TestDataset extends Dataset {

        private final List<Service> services;

        private TestDataset(String name,
                Optional<String> description,
                Optional<Path> datasetDirectory,
                Optional<String> database,
                Optional<Namespace> namespace,
                Optional<String> workspaceName,
                Optional<List<String>> externalDatasetNames,
                Optional<List<DataSubset>> dataSubsets,
                Optional<List<GeoServerStyle>> geoserverStyles,
                Optional<StaticGeoServerData> staticGeoServerData,
                Optional<List<String>> ontopMappings,
                Optional<List<String>> rules,
                Optional<List<String>> ontologyDatasetNames,
                boolean skip,
                Optional<String> rdfType,
                Optional<String> baseIRI,
                Optional<Metadata> metadataRDF, List<Service> services) {
            super(name, description, datasetDirectory, database, namespace, workspaceName, externalDatasetNames,
                    dataSubsets, geoserverStyles, staticGeoServerData, ontopMappings, rules,
                    ontologyDatasetNames, skip, rdfType, baseIRI, metadataRDF);
            this.services = services;
        }

        @Override
        boolean usesBlazegraph() {
            return services.contains(Service.BLAZEGRAPH) || super.usesBlazegraph();
        }

        @Override
        boolean usesPostGIS() {
            return (services.contains(Service.POSTGIS) || services.contains(Service.GEOSERVER)
                    || services.contains(Service.ONTOP)) || super.usesPostGIS();
        }

        @Override
        boolean usesGeoServer() {
            return services.contains(Service.GEOSERVER) || super.usesGeoServer();
        }

        @Override
        boolean usesOntop() {
            return services.contains(Service.ONTOP) || super.usesOntop();
        }

    }
}