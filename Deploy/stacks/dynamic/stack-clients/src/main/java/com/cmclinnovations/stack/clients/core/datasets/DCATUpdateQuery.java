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

    private final ModifyQuery query = Queries.MODIFY();

    private final StringLiteral currentTime = Rdf.literalOfType(LocalDateTime.now().toString(), XSD.DATETIME);

    private final Variable datasetVar = SparqlBuilder.var("dataset");
    private final Variable blazegraphServiceVar = SparqlBuilder.var("blazegraph");
    private final Variable postgisServiceVar = SparqlBuilder.var("postgis");
    private final Variable geoserverServiceVar = SparqlBuilder.var("geoserver");
    private final Variable ontopServiceVar = SparqlBuilder.var("ontop");

    String getQuery() {
        return query.getQueryString();
    }

    private Iri createIRI() {
        return Rdf.iri(SparqlConstants.DEFAULT_BASE_IRI + UUID.randomUUID());
    }

    private Variable createVar(Variable baseVar, String suffix) {
        return SparqlBuilder.var(baseVar.getVarName() + "_" + suffix);
    }

    void addDataset(Dataset dataset) {
        Variable existingDescriptionVar = createVar(datasetVar, "descripton_existing");

        query.insert(datasetVar.isA(dataset.getRdfType())
                .andHas(DCTERMS.TITLE, dataset.getName()));
        query.insert(datasetVar.has(DCTERMS.DESCRIPTION, dataset.getDescription()));

        query.delete(datasetVar.has(DCTERMS.DESCRIPTION, existingDescriptionVar));

        // Try to find the dataset but bind the new IRI if not found
        Variable existingDatasetVar = createVar(datasetVar, "existing");
        Variable issuedVar = createVar(datasetVar, "issued");
        Variable existingIssuedVar = createVar(issuedVar, "existing");
        query.where(GraphPatterns.and(
                existingDatasetVar.isA(dataset.getRdfType())
                        .andHas(DCTERMS.TITLE, dataset.getName())
                        .andHas(DCTERMS.DESCRIPTION, existingDescriptionVar)
                        .andHas(DCTERMS.ISSUED, existingIssuedVar)
                        .optional(),
                Expressions.bind(Expressions.coalesce(existingDatasetVar, createIRI()), datasetVar)),
                Expressions.bind(Expressions.coalesce(existingIssuedVar, currentTime), issuedVar));

        query.insert(datasetVar.has(DCTERMS.ISSUED, issuedVar).andHas(DCTERMS.MODIFIED, currentTime));
    }

    int externalDatasetCount = 0;

    void addExternalDataset(String externalDatasetName) {
        Variable externalDatasetVar = SparqlBuilder.var("externalDataset" + externalDatasetCount++);
        Variable oldExternalDatasetVar = SparqlBuilder.var("externalDataset_old");

        query.insert(datasetVar.has(DCAT.HAS_CATALOG, externalDatasetVar));

        query.delete(datasetVar.has(DCAT.HAS_CATALOG, oldExternalDatasetVar));

        query.where(externalDatasetVar.isA(DCAT.CATALOG).andHas(DCTERMS.TITLE, externalDatasetName));
        query.where(datasetVar.has(DCAT.HAS_CATALOG, oldExternalDatasetVar).optional());
    }

    int dataSubsetCount = 0;

    void addDataSubset(DataSubset dataSubset) {
        Variable dataSubsetVar = SparqlBuilder.var("dataSubset" + dataSubsetCount++);
        Variable issuedVar = createVar(dataSubsetVar, "issued");

        query.insert(datasetVar.has(DCAT.HAS_DATASET, dataSubsetVar));
        query.insert(dataSubsetVar.has(DCTERMS.DESCRIPTION, dataSubset.getDescription()));
        query.insert(dataSubsetVar.isA(DCAT.DATASET).andHas(DCTERMS.TITLE, dataSubset.getName()));
        query.insert(dataSubsetVar.has(DCTERMS.ISSUED, issuedVar).andHas(DCTERMS.MODIFIED, currentTime));

        Variable existingDescriptionVar = createVar(dataSubsetVar, "descripton_existing");
        query.delete(dataSubsetVar.has(DCTERMS.DESCRIPTION, existingDescriptionVar));

        // Try to find the DataSubset but bind the new IRI if not found
        Variable existingDataSubsetVar = createVar(dataSubsetVar, "existing");
        Variable existingIssuedVar = createVar(issuedVar, "existing");
        query.where(GraphPatterns.and(
                existingDataSubsetVar.isA(DCAT.DATASET)
                        .andHas(DCTERMS.TITLE, dataSubset.getName())
                        .andHas(DCTERMS.DESCRIPTION, existingDescriptionVar)
                        .andHas(DCTERMS.ISSUED, existingIssuedVar).and(
                                datasetVar.has(DCAT.HAS_DATASET, existingDataSubsetVar))
                        .optional(),
                Expressions.bind(Expressions.coalesce(existingDataSubsetVar, createIRI()), dataSubsetVar)),
                Expressions.bind(Expressions.coalesce(existingIssuedVar, currentTime), issuedVar));
    }

    void addBlazegraphServer(Dataset dataset) {
        String namespace = dataset.getNamespace();
        String url = BlazegraphClient.getInstance().getEndpoint().getUrl(namespace);

        addService(blazegraphServiceVar, namespace, SparqlConstants.BLAZEGRAPH_SERVICE, url, null);
    }

    void addPostGISServer(Dataset dataset) {
        String database = dataset.getDatabase();
        String url = PostGISClient.getInstance().getEndpoint().getJdbcURL(database);

        addService(postgisServiceVar, database, SparqlConstants.POSTGIS_SERVICE, url, null);
    }

    void addGeoServerServer(Dataset dataset) {
        addService(geoserverServiceVar, dataset.getWorkspaceName(), SparqlConstants.GEOSERVER_SERVICE, null,
                DCTERMS.REFERENCES);
    }

    void addOntopServer(Dataset dataset) {
        String ontopName = dataset.getOntopName();
        String url = OntopClient.getInstance(ontopName).getEndpoint().getUrl();
        addService(ontopServiceVar, StackClient.prependStackName(ontopName),
                SparqlConstants.ONTOP_SERVICE, url, DCTERMS.REQUIRES);
    }

    private void addService(Variable serviceVar, String title, Iri type, String url, IRI postgisRelation) {
        Variable serviceLiteral = createVar(serviceVar, "literal");

        TriplePattern serviceTriples = serviceVar.isA(type)
                .andHas(DCTERMS.TITLE, title)
                .andHas(DCTERMS.IDENTIFIER, serviceLiteral)
                .andHas(DCAT.SERVES_DATASET, datasetVar);
        if (null != url) {
            serviceTriples.andHas(DCAT.ENDPOINT_URL, Rdf.iri(url));
        }
        if (null != postgisRelation) {
            serviceTriples.andHas(postgisRelation, postgisServiceVar);
        }
        query.insert(serviceTriples);
        query.insert(datasetVar.has(DCAT.HAS_SERVICE, serviceVar));

        Variable existingServiceVar = createVar(serviceVar, "existing");
        query.delete(existingServiceVar
                .has(createVar(serviceVar, "p"),
                        createVar(serviceVar, "o")));
        Variable oldServiceVar = createVar(serviceVar, "old");
        query.delete(oldServiceVar.has(DCAT.SERVES_DATASET, datasetVar));
        query.delete(datasetVar.has(DCAT.HAS_SERVICE, oldServiceVar));

        query.where(GraphPatterns.and(
                existingServiceVar.isA(type)
                        .andHas(DCTERMS.TITLE, title)
                        .andHas(DCAT.SERVES_DATASET, datasetVar)
                        .andHas(createVar(serviceVar, "p"),
                                createVar(serviceVar, "o")))
                .optional(),
                oldServiceVar.isA(type)
                        .andHas(DCAT.SERVES_DATASET, datasetVar).optional(),
                Expressions.bind(Expressions.coalesce(existingServiceVar, createIRI()), serviceVar),
                Expressions.bind(Expressions.str(serviceVar), serviceLiteral));
    }

    public String getQueryStringForCataloging(Dataset dataset) {

        addDataset(dataset);

        // blazegraph triples
        if (dataset.usesBlazegraph()) {
            addBlazegraphServer(dataset);
        }

        if (dataset.usesPostGIS()) {
            addPostGISServer(dataset);
        }

        if (dataset.usesGeoServer()) {
            addGeoServerServer(dataset);
        }

        if (dataset.usesOntop()) {
            addOntopServer(dataset);
        }

        dataset.getDataSubsets().forEach(this::addDataSubset);

        dataset.getExternalDatasetNames().forEach(this::addExternalDataset);

        return getQuery();
    }
}