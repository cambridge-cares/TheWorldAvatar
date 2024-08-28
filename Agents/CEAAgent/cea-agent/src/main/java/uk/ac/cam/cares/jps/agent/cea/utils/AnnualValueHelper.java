package uk.ac.cam.cares.jps.agent.cea.utils;

import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;

import org.apache.jena.graph.NodeFactory;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;

import org.json.JSONArray;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AnnualValueHelper {
    /**
     * Instantiate annual value triples for CEA time series
     * @param values CEA time series values
     * @param iriMap map of the CEA data IRIs
     * @param route route to the IRIs in iriMap
     */
    public static void instantiateAnnual(List<List<?>> values, LinkedHashMap<String, String> iriMap, String route) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ub", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))
                .addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("owl", OntologyURIHelper.getOntologyUri(OntologyURIHelper.owl));

        WhereBuilder wb2 = new WhereBuilder()
                .addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology));

        int i = 0;

        List<WhereBuilder> deletes = new ArrayList<>();
        deletes.add(new WhereBuilder().addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology)));

        for (String ts: CEAConstants.TIME_SERIES) {
            if ((i+1) % 10 == 0) {
                deletes.add(new WhereBuilder().addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology)));
            }
            List<Double> value = (List<Double>) values.get(i);
            Double annualValue = 0.0;
            for (Double v : value) {
                annualValue += v;
            }
            annualValue = Math.round(annualValue*Math.pow(10,2))/Math.pow(10,2);
            String dataIRI = iriMap.get(ts);
            String energyType = getType(dataIRI, route);
            String attachedIRI = getInfo(dataIRI, ts, route);
            String measureIRI = checkAnnual(attachedIRI, energyType, route);

            if (!measureIRI.isEmpty()) {
                // if annual value already instantiated, update value
                updateAnnual(deletes.get(deletes.size()-1), wb2, measureIRI, annualValue, i);
            }
            else {
                // if no previously instantiated annual value, insert update
                insertUpdate(wb, attachedIRI, ts, energyType, annualValue);
            }
            i++;
        }

        // check if WhereBuilders have only white space characters in its where statement
        UpdateBuilder ub = new UpdateBuilder()
                .addInsert(wb);

        Pattern pattern = Pattern.compile("\\{\\s*\\}");

        Matcher m =  pattern.matcher(wb.buildString());

        m.matches();

        boolean flag = m.find();

        UpdateBuilder ub2 = new UpdateBuilder()
                .addInsert(wb2);

        Matcher m1 =  pattern.matcher(deletes.get(0).buildString());

        m1.matches();

        boolean flag1 = m1.find();

        Matcher m2 =  pattern.matcher(wb2.buildString());

        m2.matches();

        boolean flag2 = m2.find();

        // if there are no previously existing annual values
        if (!flag) {
            AccessAgentCaller.updateStore(route, ub.buildRequest().toString());
        }

        // if there are previously existing annual values
        if (!flag1 && !flag2) {
            for (WhereBuilder wb1 : deletes) {
                Matcher md =  pattern.matcher(wb1.buildString());

                md.matches();

                if (!md.find()) {
                    UpdateBuilder ub1 = new UpdateBuilder()
                            .addDelete(wb1)
                            .addWhere(wb1);
                    AccessAgentCaller.updateStore(route, ub1.buildRequest().toString());
                }
            }
            AccessAgentCaller.updateStore(route, ub2.buildRequest().toString());
        }
    }

    /**
     * Update WhereBuilders for updating existing annual value IRI
     * @param deleteWB WhereBuilder to delete old annual value
     * @param updateWB WhereBuilder to insert new annual value
     * @param iri annual value IRI
     * @param value annual value
     */
    public static void updateAnnual(WhereBuilder deleteWB, WhereBuilder updateWB, String iri, Double value, Integer counter) {
        deleteWB.addWhere(NodeFactory.createURI(iri), "om:hasNumericalValue", "?o" + counter);

        updateWB.addWhere(NodeFactory.createURI(iri), "om:hasNumericalValue", value);
    }

    /**
     * Return a query that will retrieve the quantity type of iri
     * @param iri data IRI
     * @return query string that will retrieve the quantity type of iri
     */
    public static String getType(String iri, String route) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology));

        wb.addWhere("?quantity", "om:hasValue", NodeFactory.createURI(iri));
        wb.addWhere("?quantity", "rdf:type", "?type");

        SelectBuilder sb = new SelectBuilder()
                .addVar("?type")
                .addWhere(wb);

        JSONArray queryResultArray = AccessAgentCaller.queryStore(route, sb.build().toString());
        String energyType = "";

        for (int j = 0; j < queryResultArray.length(); j++) {
            if (queryResultArray.getJSONObject(j).getString("type").contains(OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))) {
                energyType = queryResultArray.getJSONObject(j).getString("type");
                String[] split = energyType.split(OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP));
                energyType = "Annual" + split[1];
                break;
            }
        }
        return energyType;
    }

    /**
     * Returns a query that will retrieve the type of energy, i.e. heat or electricity, attached to iri
     * @param iri data IRI
     * @param energy string stating whether the data IRI is attached to a consumption or supply device
     * @return query string that will retrieve the type of energy attached to iri
     */
    public static String getInfo(String iri, String energy, String route) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ub", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))
                .addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology));

        wb.addWhere("?quantity", "om:hasValue", NodeFactory.createURI(iri));

        if (energy.contains("Consumption")) {
            wb.addWhere("?s", "ub:consumesEnergy", "?quantity");
        }
        else {
            wb.addWhere("?s", "ub:producesEnergy", "?quantity");
        }

        SelectBuilder sb = new SelectBuilder()
                .addVar("?s")
                .addWhere(wb);

        return AccessAgentCaller.queryStore(route, sb.build().toString()).getJSONObject(0).getString("s");
    }

    /**
     * Update WhereBuilder with update statements for annual value triples
     * @param wb WhereBuilder
     * @param iri data IRI
     * @param energy string stating whether the data IRI is attached to a consumption or supply device
     * @param energyType energy type, electricity or heat
     * @param value annual value
     */
    public static void insertUpdate(WhereBuilder wb, String iri, String energy, String energyType, Double value) {
        String quantity = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + "Annual" + energy + "Quantity_" + UUID.randomUUID();
        String measure = OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + "Annual" + energy + "Value_" + UUID.randomUUID();

        if (energy.contains("Consumption")) {
            wb.addWhere(NodeFactory.createURI(iri), "ub:consumesEnergy", NodeFactory.createURI(quantity));
        }
        else {
            wb.addWhere(NodeFactory.createURI(iri), "ub:producesEnergy", NodeFactory.createURI(quantity));
        }

        wb.addWhere(NodeFactory.createURI(quantity), "om:hasDimension", "om:energy-Dimension")
                .addWhere(NodeFactory.createURI(quantity), "om:hasValue", NodeFactory.createURI(measure))
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", "ub:" + energyType)
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", "owl:NamedIndividual");

        wb.addWhere(NodeFactory.createURI(measure), "om:hasUnit", "om:kilowattHour")
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "om:Measure")
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(measure), "om:hasNumericalValue", value);
    }

    /**
     * Check if annual value exists
     * @param attachedIri IRI to which the quantity IRI is attached to
     * @param energyType energy type, electricity or heat
     * @param route route to attachedIri
     * @return IRI to annual value if exists, empty string if not exists
     */
    public static String checkAnnual(String attachedIri, String energyType, String route) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ub", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))
                .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology));

        if (energyType.contains("Consumption")) {
            wb.addWhere(NodeFactory.createURI(attachedIri), "ub:consumesEnergy", "?quantity");
        }
        else {
            wb.addWhere(NodeFactory.createURI(attachedIri), "ub:producesEnergy", "?quantity");
        }

        wb.addWhere("?quantity", "rdf:type", "ub:" + energyType);
        wb.addWhere("?quantity", "om:hasValue", "?measure");

        SelectBuilder sb = new SelectBuilder()
                .addWhere(wb)
                .addVar("?measure");

        JSONArray queryResultArray = AccessAgentCaller.queryStore(route, sb.build().toString());

        if (!queryResultArray.isEmpty()) {
            return queryResultArray.getJSONObject(0).getString("measure");
        }
        else {
            return "";
        }
    }

    /**
     * Retrieves the annual value associated with attachedIri and energyType
     * @param attachedIri IRI to which the quantity IRI is attached to
     * @param energyType energy type, electricity or heat
     * @param route route to attachedIri
     * @return annual value as string
     */
    public static String retrieveAnnualValue(String attachedIri, String energyType, String route) {
        String measureIri = checkAnnual(attachedIri, energyType, route);

        if (!measureIri.isEmpty()) {
            WhereBuilder wb = new WhereBuilder()
                    .addPrefix("om", OntologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology));

            wb.addWhere(NodeFactory.createURI(measureIri), "om:hasNumericalValue", "?value");

            SelectBuilder sb = new SelectBuilder()
                    .addWhere(wb)
                    .addVar("?value");

            JSONArray queryResultArray = AccessAgentCaller.queryStore(route, sb.build().toString());

            if (!queryResultArray.isEmpty()) {
                return queryResultArray.getJSONObject(0).getString("value");
            }
            else {
                return "0.0";
            }
        }
        else {
            return "0.0";
        }
    }
}
