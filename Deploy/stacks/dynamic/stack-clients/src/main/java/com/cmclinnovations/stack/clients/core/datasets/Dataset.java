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
import org.json.JSONArray;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.blazegraph.Namespace;
import com.cmclinnovations.stack.clients.geoserver.GeoServerStyle;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.fasterxml.jackson.annotation.JacksonInject;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.derivation.ValuesPattern;

public class Dataset {
    protected static final Logger LOGGER = LoggerFactory.getLogger(Dataset.class);
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

    private final boolean skip;
    private String rdfType;

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
            @JsonProperty(value = "rdfType") String rdfType) {
        this.name = name;
        this.datasetDirectory = datasetDirectory;
        this.database = database;
        this.namespace = namespace;
        this.workspaceName = workspaceName;
        this.externalDatasetsString = externalDatasets;
        this.dataSubsets = dataSubsets;
        this.geoserverStyles = geoserverStyles;
        this.ontopMappings = ontopMappings;
        this.skip = skip;
        this.rdfType = rdfType;
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
            LOGGER.warn("Specification of database name is deprecated, name of dataset will be used, i.e. {}", getName());
        }
        return getName();
    }

    public String getNamespace() {
        if (null != namespace && null != namespace.getName()) {
            LOGGER.warn("Specification of namespace name is deprecated, name of dataset will be used, i.e. {}", getName());
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
            LOGGER.warn("Specification of workspaceName is deprecated, name of dataset will be used, i.e. {}", getName());
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

    public boolean isSkip() {
        return skip;
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
        // sets the iri if it is already initialised so that the timestamp can be modified
        setDatasetExistsAndIris();
        
        List<TriplePattern> insertTriples = new ArrayList<>();
        List<TriplePattern> deleteTriples = new ArrayList<>();

        LocalDateTime currentTime = LocalDateTime.now();
        
        SelectQuery query = Queries.SELECT(); //used to generate variable programmatically
        Iri catalogIri;
        if (!exists) {
            iri = SparqlConstants.DEFAULT_NAMESPACE + UUID.randomUUID();
            catalogIri = Rdf.iri(iri);

            insertTriples.add(catalogIri.isA(getRdfType())
            .andHas(Rdf.iri(DCTERMS.TITLE), getName())
            .andHas(Rdf.iri(DCTERMS.DESCRIPTION), getDescription())
            .andHas(Rdf.iri(DCTERMS.MODIFIED), Rdf.literalOfType(currentTime.toString(), XSD.DATETIME)));

            // blazegraph triples
            if (getDataSubsets().stream().anyMatch(DataSubset::usesBlazegraph)) {
                String kgUrl = BlazegraphClient.getInstance().getEndpoint().getUrl(getNamespace());
                Iri blazegraphService = Rdf.iri(SparqlConstants.DEFAULT_NAMESPACE + UUID.randomUUID());
                insertTriples.add(blazegraphService.isA(Rdf.iri(SparqlConstants.BLAZEGRAPH))
                .andHas(Rdf.iri(DCAT.ENDPOINT_URL), kgUrl)
                .andHas(Rdf.iri(DCAT.SERVES_DATASET), catalogIri));
            }

            Iri postgisService = null;
            if (getDataSubsets().stream().anyMatch(DataSubset::usesPostGIS)) {
                String jdbcUrl = PostGISClient.getInstance().getEndpoint().getJdbcURL(getDatabase());

                // append triples
                postgisService = Rdf.iri(SparqlConstants.DEFAULT_NAMESPACE + UUID.randomUUID());
                insertTriples.add(postgisService.isA(Rdf.iri(SparqlConstants.POSTGIS))
                .andHas(Rdf.iri(DCAT.ENDPOINT_URL), jdbcUrl)
                .andHas(Rdf.iri(SparqlConstants.HAS_DATABASE), getDatabase())
                .andHas(Rdf.iri(DCAT.SERVES_DATASET), catalogIri));
            } 
                
            // implementation not complete until we figure out the external URLs
            if (getDataSubsets().stream().anyMatch(DataSubset::usesGeoServer)) {
                Iri geoserverService = Rdf.iri(SparqlConstants.DEFAULT_NAMESPACE + UUID.randomUUID());

                // append triples
                insertTriples.add(geoserverService.isA(Rdf.iri(SparqlConstants.GEOSERVER))
                .andHas(Rdf.iri(SparqlConstants.USES_DATABASE), postgisService)
                .andHas(Rdf.iri(DCAT.SERVES_DATASET), catalogIri));
            }
    
            if (newOntopEndpoint != null) {
                Iri ontopService = Rdf.iri(SparqlConstants.DEFAULT_NAMESPACE + UUID.randomUUID());

                insertTriples.add(ontopService.isA(Rdf.iri(SparqlConstants.ONTOP))
                .andHas(Rdf.iri(SparqlConstants.USES_DATABASE), postgisService)
                .andHas(Rdf.iri(DCAT.SERVES_DATASET), catalogIri)
                .andHas(Rdf.iri(DCAT.ENDPOINT_URL), newOntopEndpoint));
            }
        } else {
            catalogIri = Rdf.iri(iri);
            Variable catalogTime = query.var();
            deleteTriples.add(catalogIri.has(DCTERMS.MODIFIED, catalogTime));

            insertTriples.add(catalogIri
            .has(DCTERMS.MODIFIED, Rdf.literalOfType(currentTime.toString(), XSD.DATETIME)));
        }

        getDataSubsets().stream().forEach(dataSubset -> {
            if (!dataSubset.getExists()) {
                Iri dataSetIri = Rdf.iri(SparqlConstants.DEFAULT_NAMESPACE + UUID.randomUUID());
                insertTriples.add(catalogIri.has(Rdf.iri(DCTERMS.HAS_PART), dataSetIri)); 
                insertTriples.add(dataSetIri.isA(Rdf.iri(DCAT.DATASET))
                .andHas(Rdf.iri(DCTERMS.MODIFIED), Rdf.literalOfType(currentTime.toString(), XSD.DATETIME))
                .andHas(Rdf.iri(DCTERMS.TITLE), dataSubset.getName())
                .andHas(Rdf.iri(DCTERMS.DESCRIPTION), dataSubset.getDescription()));
            } else {
                Variable dataSubsetTime = query.var();
                deleteTriples.add(Rdf.iri(dataSubset.getIri()).has(DCTERMS.MODIFIED, dataSubsetTime));
                insertTriples.add(Rdf.iri(dataSubset.getIri())
                .has(DCTERMS.MODIFIED, Rdf.literalOfType(currentTime.toString(), XSD.DATETIME)));
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

        GraphPattern mainQuery = GraphPatterns.and(catalogVar.has(DCTERMS.HAS_PART, dataSubsetVar)
        .andHas(Rdf.iri(DCTERMS.TITLE), catalogNameVar),
        dataSubsetVar.has(DCTERMS.TITLE, dataSubsetNameVar));

        ValuesPattern catalogValuesPattern = new ValuesPattern(catalogNameVar, Rdf.literalOf(getName()));
        ValuesPattern datasetValuesPattern = new ValuesPattern(dataSubsetNameVar, dataSubsetNames.stream().map(Rdf::literalOf).collect(Collectors.toList()));

        query.where(mainQuery, catalogValuesPattern, datasetValuesPattern).select(catalogVar, dataSubsetVar, dataSubsetNameVar);

        JSONArray queryResult = BlazegraphClient.getInstance().getRemoteStoreClient("kb").executeQuery(query.getQueryString());
        
        // catalog exists
        // used later to generate SPARQL update
        if (!queryResult.isEmpty()) {
            iri = queryResult.getJSONObject(0).getString(catalogVar.getQueryString().substring(1));
            exists = true;

            // then check each individual dataset
            Map<String, DataSubset> nameToDataSubsetMap = new HashMap<>();
            getDataSubsets().stream().forEach(dataSubset -> nameToDataSubsetMap.put(dataSubset.getName(), dataSubset));

            for (int i = 0; i < queryResult.length(); i++) {
                String queriedDatasubset = queryResult.getJSONObject(i).getString(dataSubsetVar.getQueryString().substring(1));
                String queriedDatasubsetName = queryResult.getJSONObject(i).getString(dataSubsetNameVar.getQueryString().substring(1));

                nameToDataSubsetMap.get(queriedDatasubsetName).setIri(queriedDatasubset);
                nameToDataSubsetMap.get(queriedDatasubsetName).setExists(true);
            }
        }
    }

    private class SparqlConstants {
        static final String DEFAULT_NAMESPACE = "https://www.theworldavatar.com/kg/";
        static final String BLAZEGRAPH = DEFAULT_NAMESPACE + "Blazegraph";
        static final String POSTGIS = DEFAULT_NAMESPACE + "PostGIS";
        static final String GEOSERVER = DEFAULT_NAMESPACE + "GeoServer";
        static final String USES_DATABASE = DEFAULT_NAMESPACE + "usesDatabase";
        static final String ONTOP = DEFAULT_NAMESPACE + "Ontop";
        static final String HAS_DATABASE = DEFAULT_NAMESPACE + "hasDatabase";
    }
}
