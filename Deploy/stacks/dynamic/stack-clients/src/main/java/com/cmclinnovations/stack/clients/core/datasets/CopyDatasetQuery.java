package com.cmclinnovations.stack.clients.core.datasets;

import java.util.Collection;
import java.util.stream.Collectors;

import org.eclipse.rdf4j.model.vocabulary.DCAT;
import org.eclipse.rdf4j.model.vocabulary.DCTERMS;
import org.eclipse.rdf4j.sparqlbuilder.constraint.propertypath.builder.PropertyPathBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ConstructQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;

import com.cmclinnovations.stack.clients.utils.ServiceEndpoint;

import uk.ac.cam.cares.jps.base.derivation.ValuesPattern;

public class CopyDatasetQuery {

    private static final Variable subjectVar = SparqlBuilder.var("subject");
    private static final Variable predicateVar = SparqlBuilder.var("predicate");
    private static final Variable objectVar = SparqlBuilder.var("object");

    private static final Variable serviceVar = SparqlBuilder.var("service");
    private static final Variable serviceUrlVar = SparqlBuilder.var("serviceUrl");
    private static final Variable ontologyDatasetVar = SparqlBuilder.var("ontologyDataset");

    private CopyDatasetQuery() {
    }

    public static ConstructQuery getConstructQuery(Collection<String> datasetNames) {
        return getConstructQuery(datasetNames, null);
    }

    public static ConstructQuery getConstructQuery(Collection<String> datasetNames, Iri catalogService) {
        return Queries.CONSTRUCT(getAllTriplePattern()).where(getWhere(datasetNames, catalogService));
    }

    public static ModifyQuery getInsertQuery(Collection<String> datasetNames) {
        return getInsertQuery(datasetNames, null);
    }

    public static ModifyQuery getInsertQuery(Collection<String> datasetNames, Iri catalogService) {
        return Queries.INSERT(getAllTriplePattern()).where(getWhere(datasetNames, catalogService));
    }

    private static GraphPattern getWhere(Collection<String> datasetNames, Iri catalogService) {
        ValuesPattern datasetValues = new ValuesPattern(ontologyDatasetVar,
                datasetNames.stream().map(Rdf::literalOf).collect(Collectors.toList()));

        GraphPattern where = serviceVar
                .has(PropertyPathBuilder.of(DCAT.SERVES_DATASET).then(DCTERMS.TITLE).build(), ontologyDatasetVar)
                .andIsA(SparqlConstants.BLAZEGRAPH_SERVICE)
                .andHas(DCAT.ENDPOINT_URL, serviceUrlVar)
                .and(new ServiceEndpoint(serviceUrlVar, getAllTriplePattern()))
                .and(datasetValues);

        if (catalogService != null) {
            where = new ServiceEndpoint(catalogService, where);
        }
        return where;
    }

    private static final TriplePattern getAllTriplePattern() {
        return subjectVar.has(predicateVar, objectVar);
    }
}