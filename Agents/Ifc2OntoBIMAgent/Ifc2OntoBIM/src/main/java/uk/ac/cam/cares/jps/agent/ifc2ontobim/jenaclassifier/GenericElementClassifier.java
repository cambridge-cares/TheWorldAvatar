package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaclassifier;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.StringUtils;

import java.util.*;

/**
 * Provides functions to classify the generic classes (IfcFurnishingElement, IfcBuildingElementProxy, IfcFlowTerminal)
 * into specific classes like Fridge, Table, and Sensor.
 *
 * @author qhouyee
 */
public class GenericElementClassifier {
    private static final String sparqlVar = "?element";
    /**
     * Add the mapping to relate their IRI and correct class name.
     *
     * @param placeholderClass A placeholder class that references their IfcOwl class name in the OntoBIM ontology. e.g. bim:IfcFurnishingElement
     * @param statementSet     A list containing the new OntoBIM triples.
     * @param classMapping     A mapping to relate the element to their ontoBIM class.
     */
    public static void addClassMapping(String placeholderClass, LinkedHashSet<Statement> statementSet, Map<String, String> classMapping) {
        List<Statement> statementList = new ArrayList<>(statementSet);
        Model queryModel = ModelFactory.createDefaultModel().add(statementList);
        String query = createSelectQuery(placeholderClass);
        retrieveAndAddClassMap(query, queryModel, classMapping);
    }

    /**
     * Creates the SPARQL SELECT query statements for the selected class.
     *
     * @param placeholderClass A placeholder class that references their IfcOwl class name in the OntoBIM ontology. e.g. bim:IfcFurnishingElement
     * @return A string containing the SPARQL query to execute.
     */
    private static String createSelectQuery(String placeholderClass) {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(sparqlVar)
                .addVar("?name");
        selectBuilder.addWhere(sparqlVar, "rdf:type", placeholderClass)
                .addWhere(sparqlVar, "rdfs:label", "?name");
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
            classifyClass(results.nextSolution(), classMapping);
        }
    }

    /**
     * Classify the classes according to their names and add their relationship to these mappings.
     *
     * @param soln         A row of results queried from the query execution.
     * @param classMapping A mapping to relate the element to their ontoBIM class.
     */
    private static void classifyClass(QuerySolution soln, Map<String, String> classMapping) {
        String name = soln.get("name").toString().toLowerCase().replaceAll("[\\W\\s\\d]", "");
        String element = StringUtils.getStringAfterLastCharacterOccurrence(
                soln.get(sparqlVar).toString(),"/");
        // If the IRI has an # termination character, this will ensure that it is parsed and removed
        element = StringUtils.getStringAfterLastCharacterOccurrence(element, "#");
        if (name.contains("electricalmeter")) {
            classMapping.put(element, "ElectricityMeter");
        } else if (name.contains("watermeter")) {
            classMapping.put(element, "WaterMeter");
        } else if (name.contains("oilmeter")) {
            classMapping.put(element, "OilMeter");
        } else if (name.contains("pollutionmeter")) {
            classMapping.put(element, "PollutionMeter");
        } else if (name.contains("illuminancesensor")) {
            classMapping.put(element, "IlluminanceSensor");
        } else if (name.contains("carbondioxidesensor")) {
            classMapping.put(element, "CarbonDioxideGasSensor");
        } else if (name.contains("weatherstation")) {
            classMapping.put(element, "WeatherStation");
        } else if (name.contains("solarpanel")) {
            classMapping.put(element, "SolarPanel");
        } else {
            classMapping.put(element, "Element");
        }
    }
}
