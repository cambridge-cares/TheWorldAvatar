package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import uk.ac.cam.cares.jps.base.derivation.ValuesPattern;

public class Dataset extends AbstractDataObject {

    protected static final Logger LOGGER = LoggerFactory.getLogger(Dataset.class);

    private static final String NAME_DEPRECATION_NOTICE = "Specification of '{}' name is deprecated, in the future the name of the dataset will be used, i.e. '{}' instead of '{}'";

    public static final String NAME_KEY = "name";

    @JsonProperty(value = NAME_KEY)
    @JacksonInject(NAME_KEY)
    private final String name = null;
    @JsonProperty
    private final Optional<String> description = Optional.empty();

    @JsonProperty
    private final Optional<Path> datasetDirectory = Optional.empty();

    @JsonProperty
    private final Optional<String> database = Optional.empty();
    @JsonProperty
    private final Optional<Namespace> namespace = Optional.empty();
    @JsonProperty(value = "workspace")
    private final Optional<String> workspaceName = Optional.empty();

    @JsonProperty(value = "externalDatasets")
    private final Optional<List<String>> externalDatasetsString = Optional.empty();
    @JsonIgnore
    private Optional<List<Dataset>> externalDatasets = Optional.empty();
    @JsonProperty
    private final Optional<List<DataSubset>> dataSubsets = Optional.empty();
    @JsonProperty(value = "styles")
    private final Optional<List<GeoServerStyle>> geoserverStyles = Optional.empty();
    @JsonProperty(value = "mappings")
    private final Optional<List<String>> ontopMappings = Optional.empty();
    @JsonProperty
    private final Optional<StaticGeoServerData> staticGeoServerData = Optional.empty();

    @JsonProperty
    private Optional<String> rdfType = Optional.empty();

    @JsonProperty
    private final Optional<String> baseIRI = Optional.empty();

    // for dcat cataloging
    @JsonIgnore
    private boolean exists; // used to determine whether this dataset exists in the catalog
    @JsonIgnore
    private String iri; // catalog iri

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

    public List<String> getExternalDatasetsString() {
        return externalDatasetsString.orElse(Collections.emptyList());
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

    public Optional<StaticGeoServerData> getStaticGeoServerData() {
        return staticGeoServerData;
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
        if (externalDatasets.isEmpty()) {
            externalDatasets = Optional.of(new ArrayList<>());
        }
        externalDatasets.get().add(dataset);
    }

    public List<Dataset> getExternalDatasets() {
        return externalDatasets.orElse(Collections.emptyList());
    }

    public String getQueryStringForCataloging(String newOntopEndpoint) {
        // makes a sparql query and determine which dataset is already initialised
        // sets the iri if it is already initialised so that the timestamp can be
        // modified
        setDatasetExistsAndIris();

        List<TriplePattern> insertTriples = new ArrayList<>();
        List<TriplePattern> deleteTriples = new ArrayList<>();
        List<GraphPattern> wherePatterns = new ArrayList<>();

        StringLiteral currentTime = Rdf.literalOfType(LocalDateTime.now().toString(), XSD.DATETIME);

        SelectQuery query = Queries.SELECT(); // used to generate variable programmatically
        Variable blazegraphServiceVar = query.var();
        Variable postgisServiceVar = query.var();
        Variable geoserverServiceVar = query.var();
        Variable ontopServiceVar = query.var();

        Iri catalogIri;
        if (!exists) {
            iri = SparqlConstants.DEFAULT_BASE_IRI + UUID.randomUUID();
            catalogIri = Rdf.iri(iri);

            insertTriples.add(catalogIri.isA(getRdfType())
                    .andHas(DCTERMS.TITLE, getName())
                    .andHas(DCTERMS.DESCRIPTION, getDescription())
                    .andHas(DCTERMS.ISSUED, currentTime)
                    .andHas(DCTERMS.MODIFIED, currentTime));

            // blazegraph triples
            if (getDataSubsets().stream().anyMatch(DataSubset::usesBlazegraph)) {
                String kgUrl = BlazegraphClient.getInstance().getEndpoint().getUrl(getNamespace());
                Iri blazegraphService = Rdf.iri(SparqlConstants.DEFAULT_BASE_IRI + UUID.randomUUID());
                insertTriples.add(blazegraphService.isA(SparqlConstants.BLAZEGRAPH_SERVICE)
                        .andHas(DCTERMS.TITLE, getNamespace())
                        .andHas(DCTERMS.IDENTIFIER, blazegraphService)
                        .andHas(DCAT.ENDPOINT_URL, Rdf.iri(kgUrl))
                        .andHas(DCAT.SERVES_DATASET, catalogIri));
                insertTriples.add(catalogIri.has(DCAT.HAS_SERVICE, blazegraphService));
                wherePatterns.add(new ValuesPattern(blazegraphServiceVar, blazegraphService));
            }

            if (getDataSubsets().stream().anyMatch(DataSubset::usesPostGIS)) {
                String jdbcUrl = PostGISClient.getInstance().getEndpoint().getJdbcURL(getDatabase());

                // append triples
                Iri postgisService = Rdf.iri(SparqlConstants.DEFAULT_BASE_IRI + UUID.randomUUID());
                insertTriples.add(postgisService.isA(SparqlConstants.POSTGIS_SERVICE)
                        .andHas(DCTERMS.TITLE, getDatabase())
                        .andHas(DCTERMS.IDENTIFIER, postgisService)
                        .andHas(DCAT.ENDPOINT_URL, Rdf.iri(jdbcUrl))
                        .andHas(DCAT.SERVES_DATASET, catalogIri));
                insertTriples.add(catalogIri.has(DCAT.HAS_SERVICE, postgisService));
                wherePatterns.add(new ValuesPattern(postgisServiceVar, postgisService));
            }

            // implementation not complete until we figure out the external URLs
            if (getDataSubsets().stream().anyMatch(DataSubset::usesGeoServer)) {
                Iri geoserverService = Rdf.iri(SparqlConstants.DEFAULT_BASE_IRI + UUID.randomUUID());

                // append triples
                insertTriples.add(geoserverService.isA(SparqlConstants.GEOSERVER_SERVICE)
                        .andHas(DCTERMS.TITLE, getWorkspaceName())
                        .andHas(DCTERMS.IDENTIFIER, geoserverService)
                        .andHas(DCTERMS.REFERENCES, postgisServiceVar)
                        .andHas(DCAT.SERVES_DATASET, catalogIri));
                insertTriples.add(catalogIri.has(DCAT.HAS_SERVICE, geoserverService));
                wherePatterns.add(new ValuesPattern(geoserverServiceVar, geoserverService));
            }

            if (newOntopEndpoint != null) {
                Iri ontopService = Rdf.iri(SparqlConstants.DEFAULT_BASE_IRI + UUID.randomUUID());

                insertTriples.add(ontopService.isA(SparqlConstants.ONTOP_SERVICE)
                        .andHas(DCTERMS.TITLE, getDatabase())
                        .andHas(DCTERMS.IDENTIFIER, ontopService)
                        .andHas(DCTERMS.REQUIRES, postgisServiceVar)
                        .andHas(DCAT.SERVES_DATASET, catalogIri)
                        .andHas(DCAT.ENDPOINT_URL, Rdf.iri(newOntopEndpoint)));
                insertTriples.add(catalogIri.has(DCAT.HAS_SERVICE, ontopService));
                wherePatterns.add(new ValuesPattern(ontopServiceVar, ontopService));
            }

        } else {
            catalogIri = Rdf.iri(iri);
            Variable catalogTime = query.var();
            deleteTriples.add(catalogIri.has(DCTERMS.MODIFIED, catalogTime));
            insertTriples.add(catalogIri.has(DCTERMS.MODIFIED, currentTime));
            wherePatterns.add(GraphPatterns.optional(catalogIri.has(DCAT.HAS_SERVICE, blazegraphServiceVar)
                    .and(blazegraphServiceVar.isA(SparqlConstants.BLAZEGRAPH_SERVICE))));
            wherePatterns.add(GraphPatterns.optional(catalogIri.has(DCAT.HAS_SERVICE, postgisServiceVar)
                    .and(postgisServiceVar.isA(SparqlConstants.POSTGIS_SERVICE))));
            wherePatterns.add(GraphPatterns.optional(catalogIri.has(DCAT.HAS_SERVICE, geoserverServiceVar)
                    .and(geoserverServiceVar.isA(SparqlConstants.GEOSERVER_SERVICE))));
            wherePatterns.add(GraphPatterns.optional(catalogIri.has(DCAT.HAS_SERVICE, ontopServiceVar)
                    .and(ontopServiceVar.isA(SparqlConstants.ONTOP_SERVICE))));
        }

        getDataSubsets().stream().forEach(dataSubset -> {
            if (!dataSubset.getExists()) {
                Iri dataSetIri = Rdf.iri(SparqlConstants.DEFAULT_BASE_IRI + UUID.randomUUID());
                insertTriples.add(catalogIri.has(DCAT.HAS_DATASET, dataSetIri));
                insertTriples.add(dataSetIri.isA(DCAT.DATASET)
                        .andHas(DCTERMS.ISSUED, currentTime)
                        .andHas(DCTERMS.MODIFIED, currentTime)
                        .andHas(DCTERMS.TITLE, dataSubset.getName())
                        .andHas(DCTERMS.DESCRIPTION, dataSubset.getDescription()));
                if (dataSubset.usesBlazegraph()) {
                    insertTriples.add(blazegraphServiceVar.has(DCAT.SERVES_DATASET, dataSetIri));
                }
                if (dataSubset.usesPostGIS()) {
                    insertTriples.add(postgisServiceVar.has(DCAT.SERVES_DATASET, dataSetIri));
                }
                if (dataSubset.usesGeoServer()) {
                    insertTriples.add(geoserverServiceVar.has(DCAT.SERVES_DATASET, dataSetIri));
                }
            } else {
                Variable dataSubsetTime = query.var();
                deleteTriples.add(Rdf.iri(dataSubset.getIri()).has(DCTERMS.MODIFIED, dataSubsetTime));
                insertTriples.add(Rdf.iri(dataSubset.getIri()).has(DCTERMS.MODIFIED, currentTime));
            }
        });

        getExternalDatasets().stream().forEach(externalDataset -> {
            insertTriples.add(Rdf.iri(iri).has(DCAT.HAS_CATALOG, Rdf.iri(externalDataset.getIri())));
        });

        ModifyQuery modify = Queries.MODIFY();
        TriplePattern[] insertTriplesAsArray = insertTriples.toArray(new TriplePattern[0]);
        modify.insert(insertTriplesAsArray);

        if (!deleteTriples.isEmpty()) {
            TriplePattern[] deleteTriplesAsArray = deleteTriples.toArray(new TriplePattern[0]);
            modify.delete(deleteTriplesAsArray).where(deleteTriplesAsArray);
        }

        if (!wherePatterns.isEmpty()) {
            GraphPattern[] wherePatternsAsArray = wherePatterns.toArray(new GraphPattern[0]);
            modify.where(wherePatternsAsArray);
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
                catalogVar.has(DCAT.HAS_DATASET, dataSubsetVar).andHas(DCTERMS.TITLE, catalogNameVar),
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
            iri = queryResult.getJSONObject(0).getString(catalogVar.getVarName());
            exists = true;

            // then check each individual dataset
            Map<String, DataSubset> nameToDataSubsetMap = new HashMap<>();
            getDataSubsets().stream().forEach(dataSubset -> nameToDataSubsetMap.put(dataSubset.getName(), dataSubset));

            for (int i = 0; i < queryResult.length(); i++) {
                String queriedDatasubset = queryResult.getJSONObject(i)
                        .getString(dataSubsetVar.getVarName());
                String queriedDatasubsetName = queryResult.getJSONObject(i)
                        .getString(dataSubsetNameVar.getVarName());

                nameToDataSubsetMap.get(queriedDatasubsetName).setIri(queriedDatasubset);
                nameToDataSubsetMap.get(queriedDatasubsetName).setExists(true);
            }
        }
    }
}
