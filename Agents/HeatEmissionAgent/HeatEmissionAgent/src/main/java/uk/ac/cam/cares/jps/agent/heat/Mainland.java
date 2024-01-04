package uk.ac.cam.cares.jps.agent.heat;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.json.JSONObject;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.agent.heat.objects.ChemicalPlant;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class Mainland {

    private String queryEndpoint = null;
    private String updateEndpoint = null;
    private static final String rdfs = "http://www.w3.org/2000/01/rdf-schema#";
    private static final String ontoCompanyPrefix = "http://www.theworldavatar.com/kg/ontocompany/";
    private static final String ontoMeasurePrefix = "http://www.ontology-of-units-of-measure.org/resource/om-2/";

    private List<ChemicalPlant> chemicalPlants = new ArrayList<>();

    private RemoteStoreClient storeClient;

    public Mainland() {
        EndpointConfig endpointConfig = new EndpointConfig("sgbusinessunits");
        queryEndpoint = endpointConfig.getKgurl();
        updateEndpoint = queryEndpoint;
    }

    public Mainland(String queryEndpoint, String updateEndpoint) {
        this.queryEndpoint = queryEndpoint;
        this.updateEndpoint = updateEndpoint;
    }

    public JSONObject calculateHeat() {

        storeClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);

        return new JSONObject();
    }

    private void getCompanyProperties() {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontocompany", ontoCompanyPrefix).addPrefix("rdfs", rdfs).addPrefix("om", ontoMeasurePrefix);

        String designCapacity = "?dc";
        String specificEnergyConsumption = "?se";
        wb.addWhere("?plant", "ontocompany:hasDesignCapacity/om:hasValue/om:hasNumericalValue", designCapacity)
                .addWhere("?plant", "ontocompany:hasDesignCapacity/om:hasValue/om:hasNumericalValue",
                        specificEnergyConsumption);
        SelectBuilder sb = new SelectBuilder()
                .addVar("plant")
                .addVar(designCapacity)
                .addVar(specificEnergyConsumption)
                .addWhere(wb);
        JSONArray queryResult = storeClient.executeQuery(sb.buildString());

        // Process query result

    }

}
