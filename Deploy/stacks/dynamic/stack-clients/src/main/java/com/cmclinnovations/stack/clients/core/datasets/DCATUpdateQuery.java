package com.cmclinnovations.stack.clients.core.datasets;

import java.time.LocalDateTime;
import java.util.UUID;

import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.vocabulary.DCAT;
import org.eclipse.rdf4j.model.vocabulary.DCTERMS;
import org.eclipse.rdf4j.model.vocabulary.XSD;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfLiteral.StringLiteral;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;

final class DCATUpdateQuery {

    private static final String EXISTING = "existing";

    private final ModifyQuery query = Queries.MODIFY();

    private final StringLiteral currentTime = Rdf.literalOfType(LocalDateTime.now().toString(), XSD.DATETIME);

    private final Variable datasetVar = SparqlBuilder.var("dataset");
    private final Variable blazegraphServiceVar = SparqlBuilder.var("blazegraph");
    private final Variable postgisServiceVar = SparqlBuilder.var("postgis");
    private final Variable geoserverServiceVar = SparqlBuilder.var("geoserver");
    private final Variable ontopServiceVar = SparqlBuilder.var("ontop");

    private String getQuery() {
        return query.getQueryString();
    }

    private Iri createIRI() {
        return Rdf.iri(SparqlConstants.DEFAULT_BASE_IRI + UUID.randomUUID());
    }

    private Variable createVar(Variable baseVar, String suffix) {
        return SparqlBuilder.var(baseVar.getVarName() + "_" + suffix);
    }

    private void addDataset(Dataset dataset) {

        // If there is already a description then remove it
        Variable existingDescriptionVar = createVar(datasetVar, "descripton_existing");
        query.delete(datasetVar.has(DCTERMS.DESCRIPTION, existingDescriptionVar));

        // Insert new or updated values
        Variable issuedVar = createVar(datasetVar, "issued");
        query.insert(datasetVar.isA(DCAT.CATALOG)
                .andIsA(dataset.getRdfType())
                .andHas(DCTERMS.TITLE, dataset.getName())
                .andHas(DCTERMS.DESCRIPTION, dataset.getDescription())
                // Insert new "issued" triple the first time and then just re-insert same triple
                // on subsequent runs, see comment in WHERE block.
                .andHas(DCTERMS.ISSUED, issuedVar)
                // Add new "modified" triple on each upload
                .andHas(DCTERMS.MODIFIED, currentTime));

        // Try to find the dataset but bind to a new IRI if not found
        Variable existingDatasetVar = createVar(datasetVar, EXISTING);
        Variable existingIssuedVar = createVar(issuedVar, EXISTING);
        query.where(GraphPatterns.and(
                // "rdfType" and "name" define the dataset
                existingDatasetVar.isA(DCAT.CATALOG)
                        .andIsA(dataset.getRdfType())
                        .andHas(DCTERMS.TITLE, dataset.getName())
                        .andHas(DCTERMS.DESCRIPTION, existingDescriptionVar)
                        .andHas(DCTERMS.ISSUED, existingIssuedVar)
                        .optional(),
                Expressions.bind(Expressions.coalesce(existingDatasetVar, createIRI()), datasetVar)),
                // If an issued triple already exists then keep that value otherwise use the
                // current time.
                Expressions.bind(Expressions.coalesce(existingIssuedVar, currentTime), issuedVar));
    }

    private void removeDataset(Dataset dataset) {
        Variable existingPVar = createVar(datasetVar, "_p");
        Variable existingOVar = createVar(datasetVar, "_o");

        query.delete(datasetVar.has(existingPVar, existingOVar));
        query.where(GraphPatterns.and(
                // "rdfType" and "name" define the dataset
                datasetVar.isA(DCAT.CATALOG)
                        .andIsA(dataset.getRdfType())
                        .andHas(DCTERMS.TITLE, dataset.getName())
                        .andHas(existingPVar, existingOVar)
                        .optional()));
    }

    private int referencedDatasetCount = 0;

    private void addReferencedDataset(String referencedDatasetName) {
        Variable referencedDatasetVar = SparqlBuilder.var("referencedDataset" + referencedDatasetCount++);

        // Insert link to this referenced dataset
        query.insert(datasetVar.has(DCTERMS.REFERENCES, referencedDatasetVar));

        // Find referenced dataset, this should already exist as referenced datasets are
        // loaded first
        query.where(referencedDatasetVar.isA(DCAT.CATALOG).andHas(DCTERMS.TITLE, referencedDatasetName));

    }

    private void removeExistingReferencedDatasetLinks() {
        Variable oldReferencedDatasetVar = SparqlBuilder.var("referencedDataset_old");

        // Remove all existing links to referenced datasets
        query.delete(datasetVar.has(DCTERMS.REFERENCES, oldReferencedDatasetVar));

        // Find existing links to referenced datasets
        query.where(datasetVar.has(DCTERMS.REFERENCES, oldReferencedDatasetVar).optional());
    }

    private int dataSubsetCount = 0;

    private void addDataSubset(DataSubset dataSubset) {
        Variable dataSubsetVar = SparqlBuilder.var("dataSubset" + dataSubsetCount++);
        if (!dataSubset.isSkip()) {
            Variable issuedVar = createVar(dataSubsetVar, "issued");

            // If there is already a description then remove it
            Variable existingDescriptionVar = createVar(dataSubsetVar, "description_existing");
            query.delete(dataSubsetVar.has(DCTERMS.DESCRIPTION, existingDescriptionVar));

            // Insert new or updated values
            query.insert(datasetVar.has(DCAT.HAS_DATASET, dataSubsetVar));
            query.insert(dataSubsetVar.isA(DCAT.DATASET).andHas(DCTERMS.TITLE, dataSubset.getName())
                    .andHas(DCTERMS.DESCRIPTION, dataSubset.getDescription())
                    // Insert new "issued" triple the first time and then just re-insert same triple
                    // on subsequent runs, see comment in WHERE block.
                    .andHas(DCTERMS.ISSUED, issuedVar)
                    // Add new "modified" triple on each upload
                    .andHas(DCTERMS.MODIFIED, currentTime));

            // Try to find the DataSubset but bind a new IRI if not found
            Variable existingDataSubsetVar = createVar(dataSubsetVar, EXISTING);
            Variable existingIssuedVar = createVar(issuedVar, EXISTING);
            query.where(GraphPatterns.and(
                    // "type" and "name" define the dataset
                    existingDataSubsetVar.isA(DCAT.DATASET)
                            .andHas(DCTERMS.TITLE, dataSubset.getName())
                            .andHas(DCTERMS.DESCRIPTION, existingDescriptionVar)
                            .andHas(DCTERMS.ISSUED, existingIssuedVar)
                            .and(datasetVar.has(DCAT.HAS_DATASET, existingDataSubsetVar))
                            .optional()),
                    // If an issued triple already exists then keep that value otherwise use the
                    // current time.
                    Expressions.bind(Expressions.coalesce(existingIssuedVar, currentTime), issuedVar));

            query.where(Expressions.bind(Expressions.coalesce(existingDataSubsetVar, createIRI()), dataSubsetVar));
        } else {
            // If skipped just re-add the link from the dataset.
            Variable existingDataSubsetVar = createVar(dataSubsetVar, EXISTING);
            query.insert(datasetVar.has(DCAT.HAS_DATASET, existingDataSubsetVar));
            // Try to find the DataSubset but bind a new IRI if not found
            query.where( // "type" and "name" define the dataset
                    existingDataSubsetVar.isA(DCAT.DATASET)
                            .andHas(DCTERMS.TITLE, dataSubset.getName())
                            .and(datasetVar.has(DCAT.HAS_DATASET, existingDataSubsetVar))
                            .optional());
        }
    }

    private void removeExistingDataSubsetLinks() {
        Variable oldDataSubsetVar = SparqlBuilder.var("dataSubset_old");

        // Remove all existing links to data subsets
        query.delete(datasetVar.has(DCAT.DATASET, oldDataSubsetVar));

        // Find existing links to data subsets
        query.where(datasetVar.has(DCAT.DATASET, oldDataSubsetVar).optional());
    }

    private void removeExistingDataSubsets() {
        Variable existingDataSubsetVar = SparqlBuilder.var("dataSubset_existing");
        Variable existingPVar = createVar(existingDataSubsetVar, "p");
        Variable existingOVar = createVar(existingDataSubsetVar, "o");

        query.delete(existingDataSubsetVar.has(existingPVar, existingOVar));
        query.where(existingDataSubsetVar.isA(DCAT.DATASET)
                .andHas(existingPVar, existingOVar)
                .and(datasetVar.has(DCAT.HAS_DATASET, existingDataSubsetVar))
                .optional());
    }

    private void addBlazegraphServer(Dataset dataset) {
        boolean used = dataset.usesBlazegraph();
        String namespace = dataset.getNamespace();
        String url = used ? BlazegraphClient.getInstance().readEndpointConfig().getUrl(namespace) : null;

        addService(blazegraphServiceVar, namespace, SparqlConstants.BLAZEGRAPH_SERVICE, url, null, used);
    }

    private void addPostGISServer(Dataset dataset) {
        boolean used = dataset.usesPostGIS();
        String database = dataset.getDatabase();
        String url = used ? PostGISClient.getInstance().readEndpointConfig().getJdbcURL(database) : null;

        addService(postgisServiceVar, database, SparqlConstants.POSTGIS_SERVICE, url, null, used);
    }

    private void addGeoServerServer(Dataset dataset) {
        boolean used = dataset.usesGeoServer();
        addService(geoserverServiceVar, dataset.getWorkspaceName(), SparqlConstants.GEOSERVER_SERVICE, null,
                DCTERMS.REFERENCES, used);
    }

    private void addOntopServer(Dataset dataset) {
        boolean used = dataset.usesOntop();
        String ontopName = dataset.getOntopName();
        String url = used ? OntopClient.getInstance(ontopName).readEndpointConfig().getUrl() : null;
        addService(ontopServiceVar, StackClient.prependStackName(ontopName),
                SparqlConstants.ONTOP_SERVICE, url, DCTERMS.REQUIRES, used);
    }

    private void addService(Variable serviceVar, String title, Iri type, String url, IRI postgisRelation,
            boolean isUsed) {

        if (isUsed) {
            Variable serviceLiteral = createVar(serviceVar, "literal");

            // Remove all existing triples that have this service as the subject
            Variable existingServiceVar = createVar(serviceVar, EXISTING);
            Variable existingPVar = createVar(existingServiceVar, "p");
            Variable existingOVar = createVar(existingServiceVar, "o");
            query.delete(existingServiceVar.has(existingPVar, existingOVar));

            // Insert common triples
            TriplePattern serviceTriples = serviceVar.isA(type)
                    .andHas(DCTERMS.TITLE, title)
                    .andHas(DCTERMS.IDENTIFIER, serviceLiteral)
                    .andHas(DCAT.SERVES_DATASET, datasetVar);

            // Insert optional triples
            if (null != url) {
                serviceTriples.andHas(DCAT.ENDPOINT_URL, Rdf.iri(url));
            }
            if (null != postgisRelation) {
                serviceTriples.andHas(postgisRelation, postgisServiceVar);
            }
            query.insert(serviceTriples);
            // Insert link from the dataset
            query.insert(datasetVar.has(DCAT.HAS_SERVICE, serviceVar));

            query.where(GraphPatterns.and(
                    // Look for existing instance of this service
                    existingServiceVar.isA(type)
                            .andHas(DCTERMS.TITLE, title)
                            .andHas(DCAT.SERVES_DATASET, datasetVar)
                            .andHas(existingPVar, existingOVar))
                    .optional());

            // Create new IRI for this service if not already present
            query.where(Expressions.bind(Expressions.coalesce(existingServiceVar, createIRI()), serviceVar));

            // Create literal from IRI for "identifier"
            query.where(Expressions.bind(Expressions.str(serviceVar), serviceLiteral));
        } else {
            Variable existingServiceVar = createVar(serviceVar, EXISTING);
            // Insert link from the dataset
            query.insert(datasetVar.has(DCAT.HAS_SERVICE, existingServiceVar));
            query.insert(existingServiceVar.has(DCAT.SERVES_DATASET, datasetVar));
            // Ignore this service if not already present
            query.where( // Look for existing instance of this service
                    existingServiceVar.isA(type)
                            .andHas(DCTERMS.TITLE, title)
                            .andHas(DCAT.SERVES_DATASET, datasetVar)
                            .optional());
        }
    }

    private void removeExistingServiceLinks() {
        // Remove all existing links to/from this dataset and any service
        Variable oldServiceVar = SparqlBuilder.var("serviceVar_old");
        query.delete(oldServiceVar.has(DCAT.SERVES_DATASET, datasetVar));
        query.delete(datasetVar.has(DCAT.HAS_SERVICE, oldServiceVar));

        // Look for any services linked to this dataset
        query.where(oldServiceVar.has(DCAT.SERVES_DATASET, datasetVar).optional());
    }

    private void removeExistingServices() {
        Variable existingServiceVar = SparqlBuilder.var("service_existing");
        Variable existingPVar = createVar(existingServiceVar, "p");
        Variable existingOVar = createVar(existingServiceVar, "o");

        query.delete(existingServiceVar.has(existingPVar, existingOVar));
        query.where(
                // Look for existing instance of services
                existingServiceVar.has(DCAT.SERVES_DATASET, datasetVar)
                        .andHas(existingPVar, existingOVar)
                        .optional());
    }

    private void addMetadata(Dataset dataset) {
        Metadata metadataRDF = dataset.getAdditionalMetadata();
        metadataRDF.getTriplePatterns().ifPresent(tps -> {
            metadataRDF.getPrefixes().forEach(query::prefix);
            query.insert(tps);
        });
    }

    public String getUpdateQuery(Dataset dataset) {

        addDataset(dataset);

        removeExistingReferencedDatasetLinks();
        dataset.getReferencedDatasetNames().forEach(this::addReferencedDataset);

        removeExistingDataSubsetLinks();
        dataset.getDataSubsets().stream()
                .forEach(this::addDataSubset);

        removeExistingServiceLinks();

        addBlazegraphServer(dataset);

        addPostGISServer(dataset);

        addGeoServerServer(dataset);

        addOntopServer(dataset);

        addMetadata(dataset);

        return getQuery();
    }

    public String getDeleteQuery(Dataset dataset) {

        removeDataset(dataset);

        removeExistingReferencedDatasetLinks();

        removeExistingDataSubsets();

        removeExistingServices();

        return getQuery();
    }
}