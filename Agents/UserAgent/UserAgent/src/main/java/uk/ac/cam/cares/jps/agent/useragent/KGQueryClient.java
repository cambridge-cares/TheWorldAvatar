package uk.ac.cam.cares.jps.agent.useragent;

import org.apache.jena.sparql.core.Var;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class KGQueryClient {

    private RemoteStoreClient storeClient;
    private String ontopEndpoint;
    // Prefixes
    static final String ONTOSLMA = "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/";
    static final String SLA = "https://www.theworldavatar.com/kg/sensorloggerapp/";
    static final String OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    static final String SF ="http://www.opengis.net/ont/sf#";
    static final String ONTODEVICE = "https://www.theworldavatar.com/kg/ontodevice/";
    static final String MON = "https://w3id.org/MON/person.owl";
    static final String SAREF="https://saref.etsi.org/core/";
    static final String RDF="http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    final static String str_s = "s";
    final static Var VAR_S = Var.alloc(str_s);
    final static String str_o = "o";
    final static Var VAR_O = Var.alloc(str_o);

    public KGQueryClient(RemoteStoreClient storeClient, String ontopEndpoint) {
        this.storeClient = storeClient;
        this.ontopEndpoint = ontopEndpoint;
    }

}
