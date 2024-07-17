package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import org.apache.jena.sparql.core.Var;

public class OntoConstants {
    public static final String ONTOSLMA = "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/";
    public static final String SLA = "https://www.theworldavatar.com/kg/sensorloggerapp/";
    public static final String OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String SF ="http://www.opengis.net/ont/sf#";
    public static final String ONTODEVICE = "https://www.theworldavatar.com/kg/ontodevice/";
    public static final String MON = "https://w3id.org/MON/person.owl";
    public static final String SAREF="https://saref.etsi.org/core/";
    public static final String RDF="http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    public final static String str_s = "s";
    public final static Var VAR_S = Var.alloc(str_s);
    public final static String str_o = "o";
    public final static Var VAR_O = Var.alloc(str_o);
}
