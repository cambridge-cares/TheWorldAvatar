package uk.ac.cam.cares.jps.agent.useragent;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class KGQueryClient {

    private RemoteStoreClient storeClient;
    // Prefixes
    static final String SLA = "https://www.theworldavatar.com/kg/sensorloggerapp/";
    static final String MON = "https://w3id.org/MON/person.owl";
    final Var VAR_S;
    final Var VAR_O;

    public KGQueryClient(RemoteStoreClient storeClient) {
        this.storeClient = storeClient;
        VAR_S = Var.alloc("s");
        VAR_O = Var.alloc("o");
    }

    public JSONArray getPhoneIds(String userId) {
        String userIri = MON + "#person_" + userId;
        Node userIriNode = NodeFactory.createURI(userIri);

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slm", SLA)
                .addWhere(userIriNode, "slm:hasA", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .setDistinct(true)
                .addVar(VAR_O).addWhere(wb);

        JSONArray jsonArray = storeClient.executeQuery(sb.buildString());
        JSONArray result = new JSONArray();
        for (int i = 0; i < jsonArray.length(); i++) {
            JSONObject jsonObject = jsonArray.getJSONObject(i);
            result.put(jsonObject.get("o"));
        }
        return result;
    }

}
