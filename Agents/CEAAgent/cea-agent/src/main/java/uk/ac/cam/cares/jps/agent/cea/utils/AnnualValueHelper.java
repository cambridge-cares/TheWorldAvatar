package uk.ac.cam.cares.jps.agent.cea.utils;

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
import java.util.UUID;

public class AnnualValueHelper {
    private OntologyURIHelper ontologyURIHelper;

    public AnnualValueHelper(OntologyURIHelper ontologyUriHelper) {
        this.ontologyURIHelper = ontologyUriHelper;
    }

    public void instantiateAnnual(List<List<?>> values, LinkedHashMap<String,String> iriMap, String route) {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ub", ontologyURIHelper.getOntologyUri(OntologyURIHelper.ontoUBEMMP))
                .addPrefix("om", ontologyURIHelper.getOntologyUri(OntologyURIHelper.unitOntology))
                .addPrefix("rdf", ontologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("owl", ontologyURIHelper.getOntologyUri(OntologyURIHelper.owl));

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
                    break;
                }
            }
            queryResultArray = AccessAgentCaller.queryStore(route, getInfo(dataIRI, ts).toString());
            String attachedIRI = queryResultArray.getJSONObject(0).getString("s");
            insertUpdate(wb, attachedIRI, ts, energyType, annualValue);
            i++;
        }

        UpdateBuilder ub = new UpdateBuilder()
                .addInsert(wb);

        AccessAgentCaller.updateStore(route, ub.buildRequest().toString());
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
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", NodeFactory.createURI(energyType))
                .addWhere(NodeFactory.createURI(quantity), "rdf:type", "owl:NamedIndividual");

        wb.addWhere(NodeFactory.createURI(measure), "om:hasUnit", "om:kilowattHour")
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "om:Measure")
                .addWhere(NodeFactory.createURI(measure), "rdf:type", "owl:NamedIndividual")
                .addWhere(NodeFactory.createURI(measure), "om:hasNumericalValue", value);
    }
}
