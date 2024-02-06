package uk.ac.cam.cares.jps.agent.gfaagent;

import org.json.JSONArray;
import org.json.JSONObject;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class GFAkg {
    private RemoteStoreClient kgClient;
    private String kgurl;

    public GFAkg (String kgUrl) {
        this.kgurl = kgUrl;
        this.kgClient = new RemoteStoreClient(kgUrl,kgUrl,null,null);
    }

    public void saveGFA(JSONArray gfaResult){
        String iri;
        float gfa = 0;

        for(int i = 0; i < gfaResult.length(); i++){
            JSONObject objects = gfaResult.getJSONObject(i);
            iri = kgurl + objects.getString("iri").split(".com")[1];

            gfa = objects.getFloat("GFA");

            Iri objectIri = iri(this.getObjectIri());
        }
    }
}
