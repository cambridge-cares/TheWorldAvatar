package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Properties;

import org.eclipse.rdf4j.model.vocabulary.DCAT;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.blazegraph.Namespace;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.geoserver.GeoServerStyle;
import com.cmclinnovations.stack.clients.geoserver.StaticGeoServerData;
import com.fasterxml.jackson.annotation.JacksonInject;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Dataset extends AbstractDataObject {

    protected static final Logger LOGGER = LoggerFactory.getLogger(Dataset.class);

    private static final String NAME_DEPRECATION_NOTICE = "Specification of '{}' name is deprecated, in the future the name of the dataset will be used, i.e. '{}' instead of '{}'";

    public static final String NAME_KEY = "name";

    @JsonProperty(NAME_KEY)
    @JacksonInject(NAME_KEY)
    private final String name;

    @JsonProperty
    private final Optional<Path> datasetDirectory;

    @JsonProperty
    private final Optional<String> database;
    @JsonProperty
    private final Optional<Namespace> namespace;
    @JsonProperty("workspace")
    private final Optional<String> workspaceName;

    @JsonProperty("externalDatasets")
    private final Optional<List<String>> externalDatasetNames;
    @JsonIgnore
    private final List<Dataset> externalDatasets = new ArrayList<>();
    @JsonProperty
    private final Optional<List<DataSubset>> dataSubsets;

    @JsonProperty("styles")
    private final Optional<List<GeoServerStyle>> geoserverStyles;
    @JsonProperty
    private final Optional<StaticGeoServerData> staticGeoServerData;

    @JsonProperty("mappings")
    private final Optional<List<String>> ontopMappings;

    @JsonProperty
    private final Optional<String> rdfType;
    @JsonProperty
    private final Optional<String> baseIRI;

    // for dcat cataloging
    @JsonIgnore
    private boolean exists; // used to determine whether this dataset exists in the catalog
    @JsonIgnore
    private String iri; // catalog iri

    @JsonCreator
    Dataset() {
        this.name = null;
        this.datasetDirectory = Optional.empty();
        this.database = Optional.empty();
        this.namespace = Optional.empty();
        this.workspaceName = Optional.empty();
        this.externalDatasetNames = Optional.empty();
        this.dataSubsets = Optional.empty();
        this.geoserverStyles = Optional.empty();
        this.staticGeoServerData = Optional.empty();
        this.ontopMappings = Optional.empty();
        this.rdfType = Optional.empty();
        this.baseIRI = Optional.empty();
    }

    Dataset(String name,
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
            boolean skip,
            Optional<String> rdfType,
            Optional<String> baseIRI) {
        super(description, skip);
        this.name = name;
        this.datasetDirectory = datasetDirectory;
        this.database = database;
        this.namespace = namespace;
        this.workspaceName = workspaceName;
        this.externalDatasetNames = externalDatasetNames;
        this.dataSubsets = dataSubsets;
        this.geoserverStyles = geoserverStyles;
        this.staticGeoServerData = staticGeoServerData;
        this.ontopMappings = ontopMappings;
        this.rdfType = rdfType;
        this.baseIRI = baseIRI;
    }

    public String getName() {
        return name;
    }

    public Path getDirectory() {
        return Path.of("/inputs", "data").resolve(datasetDirectory.orElse(Path.of(name)));
    }

    public String getDatabase() {
        if (database.isPresent()) {
            LOGGER.warn(NAME_DEPRECATION_NOTICE, "database", getName(), database.get());
            return database.get();
        }
        return getName();
    }

    public String getNamespace() {
        if (namespace.isPresent() && null != namespace.get().getName()) {
            LOGGER.warn(NAME_DEPRECATION_NOTICE, "namespace", getName(), namespace.get().getName());
            return namespace.get().getName();
        }
        return getName();
    }

    public Properties getNamespaceProperties() {
        if (namespace.isPresent() && null != namespace.get().getProperties()) {
            return namespace.get().getProperties();
        } else {
            return new Properties();
        }
    }

    public String getWorkspaceName() {
        if (workspaceName.isPresent()) {
            LOGGER.warn(NAME_DEPRECATION_NOTICE, "workspaceName", getName(), workspaceName.get());
            return workspaceName.get();
        }
        return getName();
    }

    public String getOntopName() {
        return getOntopMappings().isEmpty() ? null : EndpointNames.ONTOP + "-" + name;
    }

    public List<String> getExternalDatasetNames() {
        return externalDatasetNames.orElse(Collections.emptyList());
    }

    public List<DataSubset> getDataSubsets() {
        return dataSubsets.orElse(Collections.emptyList());
    }

    public List<GeoServerStyle> getGeoserverStyles() {
        return geoserverStyles.orElse(Collections.emptyList());
    }

    public List<String> getOntopMappings() {
        return ontopMappings.orElse(Collections.emptyList());
    }

    public StaticGeoServerData getStaticGeoServerData() {
        return staticGeoServerData.get();
    }

    public String baseIRI() {
        return baseIRI.orElse(SparqlConstants.DEFAULT_BASE_IRI);
    }

    public Iri getRdfType() {
        return Rdf.iri(rdfType.orElse(DCAT.CATALOG.stringValue()));
    }

    public String getIri() {
        return iri;
    }

    public void addExternalDataset(Dataset dataset) {
        externalDatasets.add(dataset);
    }

    public List<Dataset> getExternalDatasets() {
        return externalDatasets;
    }

    boolean usesBlazegraph() {
        return getDataSubsets().stream().anyMatch(DataSubset::usesBlazegraph);
    }

    boolean usesPostGIS() {
        return getDataSubsets().stream().anyMatch(DataSubset::usesPostGIS);
    }

    boolean usesGeoServer() {
        return !getGeoserverStyles().isEmpty() || getDataSubsets().stream().anyMatch(DataSubset::usesGeoServer);
    }

    boolean hasStaticGeoServerData() {
        return staticGeoServerData.isPresent();
    }

    boolean usesOntop() {
        return !getOntopMappings().isEmpty();
    }
}
