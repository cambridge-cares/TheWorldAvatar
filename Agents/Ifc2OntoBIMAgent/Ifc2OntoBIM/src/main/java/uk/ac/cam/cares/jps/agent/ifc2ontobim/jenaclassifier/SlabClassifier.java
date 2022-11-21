package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaclassifier;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.StringUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

/**
 * Provides functions to classify the IfcSlab class into its correct subtype.
 *
 * @author qhouyee
 */
public class SlabClassifier {
    private static String iriClass;
    private static String bimName;

    /**
     * Add the mapping to relate their IRI and correct class name.
     *
     * @param statementSet A list containing the new OntoBIM triples.
     * @param bimClass     Their ontoBIM class.
     * @param classMapping A mapping to relate the element to their ontoBIM class.
     */
    public static void addClassMapping(String bimClass, LinkedHashSet<Statement> statementSet, Map<String, String> classMapping) {
        List<Statement> statementList = new ArrayList<>(statementSet);
        iriClass = bimClass;
        bimName = StringUtils.getStringAfterFirstCharacterOccurrence(bimClass, ":");
        Model queryModel = ModelFactory.createDefaultModel().add(statementList);
        String query = createSelectQuery();
        retrieveAndAddClassMap(query, queryModel, classMapping);
    }

    /**
     * Creates the SPARQL SELECT query statements for the selected class.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar("?slab");
        selectBuilder.addWhere("?slab", "rdf:type", iriClass);
        return selectBuilder.buildString();
    }

    /**
     * Executes the SPARQL SELECT query on the new Model and add the returned results to the specified Map.
     *
     * @param queryString  A string containing the required SELECT statements.
     * @param queryModel   The model to query from.
     * @param classMapping A mapping to relate the element to their ontoBIM class.
     */
    private static void retrieveAndAddClassMap(String queryString, Model queryModel, Map<String, String> classMapping) {
        ResultSet results = QueryHandler.execSelectQuery(queryString, queryModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String slab = StringUtils.getStringAfterLastCharacterOccurrence(
                    soln.get("slab").toString(), "/");
            // If the IRI has an # termination character, this will ensure that it is parsed and removed
            slab = StringUtils.getStringAfterLastCharacterOccurrence(slab, "#");
            classMapping.put(slab, bimName);
        }
    }
}
