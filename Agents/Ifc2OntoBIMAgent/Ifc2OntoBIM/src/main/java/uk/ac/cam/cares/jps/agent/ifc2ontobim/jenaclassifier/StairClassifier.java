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
 * Provides functions to classify the sub-elements of the Stair class into its correct subtype.
 *
 * @author qhouyee
 */
public class StairClassifier {
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
        selectBuilder.addVar("?stair")
                .addVar("?landing")
                .addVar("?structurecomponent");
        selectBuilder.addWhere("?stair", "rdf:type", "bim:Stair")
                .addWhere("?stair", "bim:hasStairSubElement", "?landing")
                .addWhere("?landing", "rdf:type", "bim:Landing")
                .addWhere("?stair", "bim:hasStairSubElement", "?structurecomponent")
                .addWhere("?structurecomponent", "rdf:type", "bim:StructuralComponent");
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
            String landing = StringUtils.getStringAfterLastCharacterOccurrence(
                    soln.get("landing").toString(), "/");
            String structurecomponent = StringUtils.getStringAfterLastCharacterOccurrence(
                    soln.get("structurecomponent").toString(), "/");
            // If the IRI has an # termination character, this will ensure that it is parsed and removed
            landing = StringUtils.getStringAfterLastCharacterOccurrence(landing, "#");
            structurecomponent = StringUtils.getStringAfterLastCharacterOccurrence(structurecomponent, "#");
            classMapping.put(landing, "Landing");
            classMapping.put(structurecomponent, "StructuralComponent");
        }
    }
}

