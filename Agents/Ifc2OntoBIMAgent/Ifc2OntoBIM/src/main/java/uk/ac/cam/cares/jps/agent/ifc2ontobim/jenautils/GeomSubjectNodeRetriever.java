package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.*;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * Provides methods to retrieve the geometry subject nodes from the input statement set.
 *
 * @author qhouyee
 */
public class GeomSubjectNodeRetriever {
    private final String geomVariable;
    private final String geomClass;

    /**
     * Standard Constructor to initialise the variables.
     *
     * @param geomVariable A text containing the variable, usually appended to ? in SPARQL queries ie ?geomvariable.
     * @param geomClass    A text containing the geometry class in ontoBIM.
     */
    public GeomSubjectNodeRetriever(String geomVariable, String geomClass) {
        this.geomVariable = geomVariable;
        this.geomClass = geomClass;
    }

    /**
     * The public interface that create a temporary model containing the present triples and
     * executes the required method to retrieve IRIs belonging to the selected class.
     *
     * @param statementSet A list containing the new OntoBIM triples.
     * @return A list of IRIs that is returned from the query.
     */
    public List<RDFNode> retrieveIriAsList(LinkedHashSet<Statement> statementSet) {
        List<Statement> statementList = new ArrayList<>(statementSet);
        Model queryModel = ModelFactory.createDefaultModel().add(statementList);
        String query = this.createSelectQuery();
        return this.retrieveNodeList(query, queryModel);
    }

    /**
     * Creates the SPARQL SELECT query statements for the selected class.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private String createSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        String sparqlVariable = "?" + this.geomVariable;
        selectBuilder.addVar(sparqlVariable);
        selectBuilder.addWhere(sparqlVariable, QueryHandler.RDF_TYPE, this.geomClass);
        return selectBuilder.buildString();
    }

    /**
     * Retrieve and store the SPARQL SELECT query results in a List.
     *
     * @param queryString A string containing the required SELECT statements.
     * @param queryModel  The query model to retrieve statements from.
     * @return A list of IRIs that is returned from the query.
     */
    private List<RDFNode> retrieveNodeList(String queryString, Model queryModel) {
        List<RDFNode> queryResults = new ArrayList<>();
        ResultSet results = QueryHandler.execSelectQuery(queryString, queryModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            RDFNode iri = soln.get(this.geomVariable);
            queryResults.add(iri);
        }
        return queryResults;
    }
}
