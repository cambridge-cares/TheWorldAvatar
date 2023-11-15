package uk.ac.cam.cares.jps.agent.cea.utils;

import org.apache.jena.arq.querybuilder.handlers.WhereHandler;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;

import org.apache.jena.graph.NodeFactory;
import org.apache.jena.query.Query;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;

import org.json.JSONArray;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AnnualValueHelper {
    private OntologyURIHelper ontologyURIHelper;

    public AnnualValueHelper(OntologyURIHelper ontologyUriHelper) {
        this.ontologyURIHelper = ontologyUriHelper;
    }

    public void instantiateAnnual(List<List<?>> values, LinkedHashMap<String, String> iriMap, String route) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ub", ontologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))
                .addPrefix("om", ontologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                .addPrefix("rdf", ontologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("owl", ontologyURIHelper.getOntologyUri(OntologyURIHelper.owl));

        WhereBuilder wb1 = new WhereBuilder()
                .addPrefix("om", ontologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology));
        WhereBuilder wb2 = new WhereBuilder()
                .addPrefix("om", ontologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology));

        int i = 0;

        for (String ts: CEAConstants.TIME_SERIES) {
            List<Double> value = (List<Double>) values.get(i);
            Double annualValue = 0.0;
            for (Double v : value) {
                annualValue += v;
            }
            annualValue = Math.round(annualValue*Math.pow(10,2))/Math.pow(10,2);
            String dataIRI = iriMap.get(ts);
            JSONArray queryResultArray = AccessAgentCaller.queryStore(route, getType(dataIRI).toString());
            String energyType = "";

            for (int j = 0; j < queryResultArray.length(); j++) {
                if (queryResultArray.getJSONObject(j).getString("type").contains(ontologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))) {
                    energyType = queryResultArray.getJSONObject(j).getString("type");
                    String[] split = energyType.split(ontologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP));
                    energyType = "Annual" + split[1];
                    break;
                }
            }
            queryResultArray = AccessAgentCaller.queryStore(route, getInfo(dataIRI, ts).toString());
            String attachedIRI = queryResultArray.getJSONObject(0).getString("s");
            String measureIRI = checkAnnual(attachedIRI, energyType, route);

            if (!measureIRI.isEmpty()) {
                // if annual value already instantiated, update value
                updateAnnual(wb1, wb2, measureIRI, annualValue);
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

        UpdateBuilder ub1 = new UpdateBuilder()
                .addDelete(wb1)
                .addWhere(wb1);
        UpdateBuilder ub2 = new UpdateBuilder()
                .addInsert(wb2);

        Matcher m1 =  pattern.matcher(wb1.buildString());

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
            AccessAgentCaller.updateStore(route, ub1.buildRequest().toString());
            AccessAgentCaller.updateStore(route, ub2.buildRequest().toString());
        }
    }

    public void updateAnnual(WhereBuilder deleteWB, WhereBuilder updateWB, String iri, Double value) {
        deleteWB.addWhere(NodeFactory.createURI(iri), "om:hasNumericalValue", "?o");

        updateWB.addWhere(NodeFactory.createURI(iri), "om:hasNumericalValue", value);
    }

    public Query getType(String iri) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("rdf", ontologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("om", ontologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology));

        wb.addWhere("?quantity", "om:hasValue", NodeFactory.createURI(iri));
        wb.addWhere("?quantity", "rdf:type", "?type");

        SelectBuilder sb = new SelectBuilder()
                .addVar("?type")
                .addWhere(wb);

        return sb.build();
    }

    public Query getInfo(String iri, String energy) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ub", ontologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))
                .addPrefix("om", ontologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology));

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

        return sb.build();
    }

    public void insertUpdate(WhereBuilder wb, String iri, String energy, String energyType, Double value) {
        String quantity = ontologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + "Annual" + energy + "Quantity_" + UUID.randomUUID();
        String measure = ontologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP) + "Annual" + energy + "Value_" + UUID.randomUUID();

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

    public String checkAnnual(String iri, String energyType, String route) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ub", ontologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))
                .addPrefix("rdf", ontologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("om", ontologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology));

        if (energyType.contains("Consumption")) {
            wb.addWhere(NodeFactory.createURI(iri), "ub:consumesEnergy", "?quantity");
        }
        else {
            wb.addWhere(NodeFactory.createURI(iri), "ub:producesEnergy", "?quantity");
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
}
