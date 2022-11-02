package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;

import java.util.*;

/**
 * Provides the functions to initialise a SPARQL query builder, execute all SPARQL query types, and provides common properties.
 *
 * @author qhouyee
 */
public class QueryHandler {
    public static final String RDF_TYPE = "rdf:type";
    public static final String RDFS_LABEL = "rdfs:label";

    /**
     * Initialise a SPARQL SELECT query builder.
     *
     * @return The SELECT builder.
     */
    public static SelectBuilder initSelectQueryBuilder() {
        SelectBuilder selectBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(selectBuilder);
        selectBuilder.setDistinct(true);
        return selectBuilder;
    }

    /**
     * Executes the SPARQL SELECT query on the new Model.
     *
     * @param queryString A string containing the required SELECT statements.
     * @param queryModel  The query model to retrieve statements from.
     * @return The query results for further processing.
     */
    public static ResultSet execSelectQuery(String queryString, Model queryModel) {
        Query query = QueryFactory.create(queryString);
        try (QueryExecution qExec = QueryExecutionFactory.create(query, queryModel)) {
            return ResultSetFactory.copyResults(qExec.execSelect());
        }
    }

    /**
     * Executes a Sparql Construct query and add the statements returned into a Linked Hash Set.
     *
     * @param queryString  A string containing the SPARQL query.
     * @param queryModel   The query model to retrieve statements from.
     * @param statementSet An ordered set to store the statements from the query results.
     */
    public static void queryConstructStatementsAsSet(String queryString, Model queryModel, LinkedHashSet<Statement> statementSet) {
        Query query = QueryFactory.create(queryString);
        try (QueryExecution qExec = QueryExecutionFactory.create(query, queryModel)) {
            Model results = qExec.execConstruct();
            results.listStatements().forEach(statementSet::add);
        }
    }
}
