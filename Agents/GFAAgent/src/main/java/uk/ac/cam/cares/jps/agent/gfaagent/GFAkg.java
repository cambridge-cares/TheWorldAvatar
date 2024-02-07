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
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.apache.jena.graph.Triple;

public class GFAkg {
    private RemoteStoreClient kgClient;
    private String kgurl;
    static String ontogfa = "http://www.theworldavatar.com/ontology/ontogfa/OntoGFA.owl#";
    static Prefix p_building = SparqlBuilder.prefix("building",iri(ontogfa));
    static Iri objBuilding = p_building.iri("Building");
    static Prefix p_gfa = SparqlBuilder.prefix("GFA",iri(ontogfa));
    static Iri hasGFA = p_gfa.iri("GFA");
    static Prefix p_r_gfa = SparqlBuilder.prefix("referenceGFA",iri(ontogfa));
    static Iri hasRefGFA = p_r_gfa.iri("referenceGFA");

    public GFAkg (String kgUrl) {
        this.kgurl = kgUrl;
        this.kgClient = new RemoteStoreClient(kgUrl,kgUrl,null,null);;
    }

    public void saveGFA(JSONArray gfaResult){
        String iri;
        float gfa = 0;

        for(int i = 0; i < gfaResult.length(); i++){
            JSONObject objects = gfaResult.getJSONObject(i);
            iri = objects.getString("iri");
            gfa = objects.getFloat("gfa");

            InsertDataQuery insertIri = Queries.INSERT_DATA();
            InsertDataQuery insertGfa = Queries.INSERT_DATA();

            Iri objectIri = iri(iri);
            insertIri.insertData(objectIri.isA(objBuilding));
            insertIri.prefix(p_building);

            insertGfa.insertData(objectIri.has(hasGFA, gfa));
            insertGfa.prefix(p_gfa);

            this.kgClient.executeUpdate(insertIri.getQueryString());
            this.kgClient.executeUpdate(insertGfa.getQueryString());
        }
    }
}
