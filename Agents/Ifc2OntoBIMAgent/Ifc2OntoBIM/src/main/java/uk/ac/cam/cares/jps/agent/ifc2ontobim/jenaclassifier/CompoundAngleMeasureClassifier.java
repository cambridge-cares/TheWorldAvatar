package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaclassifier;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.StringUtils;

import java.util.*;

/**
 * Provides functions to classify the Latitude and Longitude classes retrieved in IfcSite.
 *
 * @author qhouyee
 */
public class CompoundAngleMeasureClassifier {
    /**
     * Add the mapping to relate their IRI and correct class name.
     *
     * @param statementSet A list containing the new OntoBIM triples.
     * @param classMapping A mapping to relate the element to their ontoBIM class.
     */
    public static void addClassMapping(LinkedHashSet<Statement> statementSet, Map<String, String> classMapping) {
        List<Statement> statementList = new ArrayList<>(statementSet);
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
        selectBuilder.addVar("?latitude")
                .addVar("?longitude");
        selectBuilder.addWhere("?latitude", "rdf:type", "bim:Latitude")
                .addWhere("?longitude", "rdf:type", "bim:Longitude");
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
            String latitude = StringUtils.getStringAfterLastCharacterOccurrence(
                    soln.get("latitude").toString(), "/");
            String longitude = StringUtils.getStringAfterLastCharacterOccurrence(
                    soln.get("longitude").toString(), "/");
            // If the IRI has an # termination character, this will ensure that it is parsed and removed
            latitude = StringUtils.getStringAfterLastCharacterOccurrence(latitude, "#");
            longitude = StringUtils.getStringAfterLastCharacterOccurrence(longitude, "#");
            classMapping.put(latitude, "Latitude");
            classMapping.put(longitude, "Longitude");
        }
    }
}
