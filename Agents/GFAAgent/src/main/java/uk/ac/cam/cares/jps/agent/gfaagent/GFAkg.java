package uk.ac.cam.cares.jps.agent.gfaagent;

import org.json.JSONArray;
import org.json.JSONObject;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

public class GFAkg {
    private RemoteStoreClient kgClient;
    private String kgurl;

    public GFAkg (String kgUrl) {
        this.kgurl = kgUrl;
        this.kgClient = new RemoteStoreClient(kgUrl,kgUrl,null,null);;
    }

    public void saveGFA(JSONArray gfaResult){
        String iri;
        float gfa = 0;

        for(int i = 0; i < gfaResult.length(); i++){
            JSONObject objects = gfaResult.getJSONObject(i);
            iri = kgurl + objects.getString("iri").split(".com/")[1];
            gfa = objects.getFloat("gfa");

            InsertDataQuery modify = Queries.INSERT_DATA();
            Iri objectIri = iri(iri);

        }
    }
}
