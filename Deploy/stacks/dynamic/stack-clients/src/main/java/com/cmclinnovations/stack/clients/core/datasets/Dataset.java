package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.stream.Collectors;

import org.eclipse.rdf4j.model.vocabulary.DCAT;
import org.eclipse.rdf4j.model.vocabulary.DCTERMS;
import org.eclipse.rdf4j.model.vocabulary.XSD;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfLiteral.StringLiteral;
import org.json.JSONArray;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.blazegraph.Namespace;
import com.cmclinnovations.stack.clients.geoserver.GeoServerStyle;
import com.cmclinnovations.stack.clients.geoserver.StaticGeoServerData;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.fasterxml.jackson.annotation.JacksonInject;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import uk.ac.cam.cares.jps.base.derivation.ValuesPattern;

public class Dataset {

    protected static final Logger LOGGER = LoggerFactory.getLogger(Dataset.class);

    private static final String NAME_DEPRECATION_NOTICE = "Specification of '{}' name is deprecated, in the future the name of the dataset will be used, i.e. {}";

    static final String DEFAULT_NAMESPACE = "https://www.theworldavatar.com/kg/";
    static final String DEFAULT_BASE_IRI = "https://www.theworldavatar.com/kg/";

    public static final String NAME_KEY = "name";

    private String name;
    private String description;

    private final Path datasetDirectory;

    private final String database;
    private final Namespace namespace;
    private final String workspaceName;

    private final List<String> externalDatasetsString;
    private List<Dataset> externalDatasets;
    private final List<DataSubset> dataSubsets;
    private final List<GeoServerStyle> geoserverStyles;
    private final List<String> ontopMappings;
    private final StaticGeoServerData staticGeoServerData;

    private final boolean skip;
    private String rdfType;

    private final String baseIRI;

    // for dcat cataloging
    private boolean exists; // used to determine whether this dataset exists in the catalog
    private String iri; // catalog iri

    @JsonCreator
    public Dataset(@JsonProperty(value = NAME_KEY) @JacksonInject(NAME_KEY) String name,
            @JsonProperty(value = "datasetDirectory") Path datasetDirectory,
            @JsonProperty(value = "database") String database,
            @JsonProperty(value = "namespace") Namespace namespace,
            @JsonProperty(value = "workspace") String workspaceName,
            @JsonProperty(value = "externalDatasets") List<String> externalDatasets,
            @JsonProperty(value = "dataSubsets") List<DataSubset> dataSubsets,
            @JsonProperty(value = "styles") List<GeoServerStyle> geoserverStyles,
            @JsonProperty(value = "mappings") List<String> ontopMappings,
            @JsonProperty(value = "skip") boolean skip,
            @JsonProperty(value = "staticGeoServerData") StaticGeoServerData staticGeoServerData,
            @JsonProperty(value = "rdfType") String rdfType,
            @JsonProperty(value = "baseIRI") String baseIRI) {
        this.name = name;
        this.datasetDirectory = datasetDirectory;
        this.database = database;
        this.namespace = namespace;
        this.workspaceName = workspaceName;
        this.externalDatasetsString = externalDatasets;
        this.dataSubsets = dataSubsets;
        this.geoserverStyles = geoserverStyles;
        this.ontopMappings = ontopMappings;
        this.staticGeoServerData = staticGeoServerData;
        this.skip = skip;
        this.rdfType = rdfType;
        this.baseIRI = (null == baseIRI) ? DEFAULT_BASE_IRI : baseIRI;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return (null != description) ? description : getName();
    }

    public Path getDirectory() {
        return Path.of("/inputs", "data").resolve((null != datasetDirectory) ? datasetDirectory : Path.of(name));
    }

    public String getDatabase() {
        if (database != null) {
            LOGGER.warn(NAME_DEPRECATION_NOTICE, "database", getName());
            return database;
        }
        return getName();
    }

    public String getNamespace() {
        if (null != namespace && null != namespace.getName()) {
            LOGGER.warn(NAME_DEPRECATION_NOTICE, "namespace", getName());
            return namespace.getName();
        }
        return getName();
    }

    public Properties getNamespaceProperties() {
        if (null != namespace && null != namespace.getProperties()) {
            return namespace.getProperties();
        } else {
            return new Properties();
        }
    }

    public String getWorkspaceName() {
        if (workspaceName != null) {
            LOGGER.warn(NAME_DEPRECATION_NOTICE, "workspaceName", getName());
            return workspaceName;
        }
        return getName();
    }

    public List<String> getExternalDatasetsString() {
        return (null != externalDatasetsString) ? externalDatasetsString : Collections.emptyList();
    }

    public List<DataSubset> getDataSubsets() {
        return (null != dataSubsets) ? dataSubsets : Collections.emptyList();
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

    public String baseIRI() {
        return baseIRI;
    }

    public Iri getRdfType() {
        return (null != rdfType) ? Rdf.iri(rdfType) : Rdf.iri(DCAT.CATALOG);
    }

    public String getIri() {
        return iri;
    }

    public void addExternalDataset(Dataset dataset) {
        if (externalDatasets == null) {
            externalDatasets = new ArrayList<>();
        }
        externalDatasets.add(dataset);
    }

    public List<Dataset> getExternalDatasets() {
        return (null != externalDatasets) ? externalDatasets : Collections.emptyList();
    }

    public String getQueryStringForCataloging(String newOntopEndpoint) {
        // makes a sparql query and determine which dataset is already initialised
        // sets the iri if it is already initialised so that the timestamp can be
        // modified
        setDatasetExistsAndIris();

        List<TriplePattern> insertTriples = new ArrayList<>();
        List<TriplePattern> deleteTriples = new ArrayList<>();

        StringLiteral currentTime = Rdf.literalOfType(LocalDateTime.now().toString(), XSD.DATETIME);

        SelectQuery query = Queries.SELECT(); // used to generate variable programmatically
        Iri catalogIri;
        if (!exists) {
            iri = SparqlConstants.DEFAULT_NAMESPACE + UUID.randomUUID();
            catalogIri = Rdf.iri(iri);

            insertTriples.add(catalogIri.isA(getRdfType())
                    .andHas(DCTERMS.TITLE, getName())
                    .andHas(DCTERMS.DESCRIPTION, getDescription())
                    .andHas(DCTERMS.MODIFIED, currentTime));

            // blazegraph triples
            if (getDataSubsets().stream().anyMatch(DataSubset::usesBlazegraph)) {
                String kgUrl = BlazegraphClient.getInstance().getEndpoint().getUrl(getNamespace());
                Iri blazegraphService = Rdf.iri(SparqlConstants.DEFAULT_NAMESPACE + UUID.randomUUID());
                insertTriples.add(blazegraphService.isA(SparqlConstants.BLAZEGRAPH)
                        .andHas(DCAT.ENDPOINT_URL, Rdf.iri(kgUrl))
                        .andHas(DCAT.SERVES_DATASET, catalogIri));
            }

            Iri postgisService = null;
            if (getDataSubsets().stream().anyMatch(DataSubset::usesPostGIS)) {
                String jdbcUrl = PostGISClient.getInstance().getEndpoint().getJdbcURL(getDatabase());

                // append triples
                postgisService = Rdf.iri(SparqlConstants.DEFAULT_NAMESPACE + UUID.randomUUID());
                insertTriples.add(postgisService.isA(SparqlConstants.POSTGIS)
                        .andHas(DCAT.ENDPOINT_URL, Rdf.iri(jdbcUrl))
                        .andHas(SparqlConstants.HAS_DATABASE, getDatabase())
                        .andHas(DCAT.SERVES_DATASET, catalogIri));
            }

            // implementation not complete until we figure out the external URLs
            if (getDataSubsets().stream().anyMatch(DataSubset::usesGeoServer)) {
                Iri geoserverService = Rdf.iri(SparqlConstants.DEFAULT_NAMESPACE + UUID.randomUUID());

                // append triples
                insertTriples.add(geoserverService.isA(SparqlConstants.GEOSERVER)
                        .andHas(SparqlConstants.USES_DATABASE, postgisService)
                        .andHas(DCAT.SERVES_DATASET, catalogIri));
            }

            if (newOntopEndpoint != null) {
                Iri ontopService = Rdf.iri(SparqlConstants.DEFAULT_NAMESPACE + UUID.randomUUID());

                insertTriples.add(ontopService.isA(SparqlConstants.ONTOP)
                        .andHas(SparqlConstants.USES_DATABASE, postgisService)
                        .andHas(DCAT.SERVES_DATASET, catalogIri)
                        .andHas(DCAT.ENDPOINT_URL, Rdf.iri(newOntopEndpoint)));
            }
        } else {
            catalogIri = Rdf.iri(iri);
            Variable catalogTime = query.var();
            deleteTriples.add(catalogIri.has(DCTERMS.MODIFIED, catalogTime));
            insertTriples.add(catalogIri.has(DCTERMS.MODIFIED, currentTime));
        }

        getDataSubsets().stream().forEach(dataSubset -> {
            if (!dataSubset.getExists()) {
                Iri dataSetIri = Rdf.iri(SparqlConstants.DEFAULT_NAMESPACE + UUID.randomUUID());
                insertTriples.add(catalogIri.has(DCTERMS.HAS_PART, dataSetIri));
                insertTriples.add(dataSetIri.isA(DCAT.DATASET)
                        .andHas(DCTERMS.MODIFIED, currentTime)
                        .andHas(DCTERMS.TITLE, dataSubset.getName())
                        .andHas(DCTERMS.DESCRIPTION, dataSubset.getDescription()));
            } else {
                Variable dataSubsetTime = query.var();
                deleteTriples.add(Rdf.iri(dataSubset.getIri()).has(DCTERMS.MODIFIED, dataSubsetTime));
                insertTriples.add(Rdf.iri(dataSubset.getIri()).has(DCTERMS.MODIFIED, currentTime));
            }
        });

        getExternalDatasets().stream().forEach(externalDataset -> {
            insertTriples.add(Rdf.iri(iri).has(DCTERMS.HAS_PART, Rdf.iri(externalDataset.getIri())));
        });

        ModifyQuery modify = Queries.MODIFY();
        TriplePattern[] insertTriplesAsArray = insertTriples.toArray(new TriplePattern[insertTriples.size()]);
        modify.insert(insertTriplesAsArray);

        if (!deleteTriples.isEmpty()) {
            TriplePattern[] deleteTriplesAsArray = deleteTriples.toArray(new TriplePattern[deleteTriples.size()]);
            modify.delete(deleteTriplesAsArray).where(deleteTriplesAsArray);
        }

        return modify.getQueryString();
    }

    /**
     * goes through catalog name and dataset names and check if they exist
     */
    private void setDatasetExistsAndIris() {
        List<String> dataSubsetNames = getDataSubsets().stream().map(DataSubset::getName).collect(Collectors.toList());

        SelectQuery query = Queries.SELECT();
        Variable catalogVar = query.var();
        Variable catalogNameVar = query.var();
        Variable dataSubsetVar = query.var();
        Variable dataSubsetNameVar = query.var();

        GraphPattern mainQuery = GraphPatterns.and(
                catalogVar.has(DCTERMS.HAS_PART, dataSubsetVar).andHas(DCTERMS.TITLE, catalogNameVar),
                dataSubsetVar.has(DCTERMS.TITLE, dataSubsetNameVar));

        ValuesPattern catalogValuesPattern = new ValuesPattern(catalogNameVar, Rdf.literalOf(getName()));
        ValuesPattern datasetValuesPattern = new ValuesPattern(dataSubsetNameVar,
                dataSubsetNames.stream().map(Rdf::literalOf).collect(Collectors.toList()));

        query.where(mainQuery, catalogValuesPattern, datasetValuesPattern).select(catalogVar, dataSubsetVar,
                dataSubsetNameVar);

        JSONArray queryResult = BlazegraphClient.getInstance().getRemoteStoreClient("kb")
                .executeQuery(query.getQueryString());

        // catalog exists
        // used later to generate SPARQL update
        if (!queryResult.isEmpty()) {
            iri = queryResult.getJSONObject(0).getString(catalogVar.getQueryString().substring(1));
            exists = true;

            // then check each individual dataset
            Map<String, DataSubset> nameToDataSubsetMap = new HashMap<>();
            getDataSubsets().stream().forEach(dataSubset -> nameToDataSubsetMap.put(dataSubset.getName(), dataSubset));

            for (int i = 0; i < queryResult.length(); i++) {
                String queriedDatasubset = queryResult.getJSONObject(i)
                        .getString(dataSubsetVar.getQueryString().substring(1));
                String queriedDatasubsetName = queryResult.getJSONObject(i)
                        .getString(dataSubsetNameVar.getQueryString().substring(1));

                nameToDataSubsetMap.get(queriedDatasubsetName).setIri(queriedDatasubset);
                nameToDataSubsetMap.get(queriedDatasubsetName).setExists(true);
            }
        }
    }

    private static class SparqlConstants {
        static final String DEFAULT_NAMESPACE = "https://www.theworldavatar.com/kg/";
        static final Iri BLAZEGRAPH = Rdf.iri(DEFAULT_NAMESPACE + "Blazegraph");
        static final Iri POSTGIS = Rdf.iri(DEFAULT_NAMESPACE + "PostGIS");
        static final Iri GEOSERVER = Rdf.iri(DEFAULT_NAMESPACE + "GeoServer");
        static final Iri USES_DATABASE = Rdf.iri(DEFAULT_NAMESPACE + "usesDatabase");
        static final Iri ONTOP = Rdf.iri(DEFAULT_NAMESPACE + "Ontop");
        static final Iri HAS_DATABASE = Rdf.iri(DEFAULT_NAMESPACE + "hasDatabase");
    }
}
