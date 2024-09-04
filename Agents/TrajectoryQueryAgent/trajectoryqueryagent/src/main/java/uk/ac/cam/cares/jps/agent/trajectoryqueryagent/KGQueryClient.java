package uk.ac.cam.cares.jps.agent.trajectoryqueryagent;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class KGQueryClient {

    private RemoteStoreClient storeClient;
    // Prefixes
    static final String ONTOSLMA = "https://www.theworldavatar.com/kg/ontosensorloggermobileapp/";
    static final String SLA = "https://www.theworldavatar.com/kg/sensorloggerapp/";
    static final String OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    static final String SF = "http://www.opengis.net/ont/sf#";
    static final String ONTODEVICE = "https://www.theworldavatar.com/kg/ontodevice/";
    static final String MON = "https://w3id.org/MON/person.owl";
    static final String SAREF = "https://saref.etsi.org/core/";
    static final String RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    final static String str_s = "s";
    final static Var VAR_S = Var.alloc(str_s);
    final static String str_o = "o";
    final static Var VAR_O = Var.alloc(str_o);

    public KGQueryClient(RemoteStoreClient storeClient) {
        this.storeClient = storeClient;
    }

    String getIRIfromJSONarray(JSONArray jsonArray) {
        if (jsonArray.length() == 0) {
            System.out.println("KGQueryClient: No iri retrieved");
            return null;
        }
        if (jsonArray.length() > 1) {
            System.out.println("There is more than 1 data in this JSONArray, only the first one is returned");
        }
        return jsonArray.getJSONObject(0).get(str_o).toString();
    }

    JSONArray getBearingIRIArray(Node smartphoneIRI) {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRI, "saref:consistsOf", "?GPSDevice")
                .addWhere("?GPSDevice", "rdf:type", "ontodevice:GPSDevice")
                .addWhere("?GPSDevice", "ontodevice:measures", "?Bearing")
                .addWhere("?Bearing", "rdf:type", "slma:Bearing")
                .addWhere("?Bearing", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
        return queryResult;
    }

    JSONArray getAltitudeIRIArray(Node smartphoneIRI) {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRI, "saref:consistsOf", "?GPSDevice")
                .addWhere("?GPSDevice", "rdf:type", "ontodevice:GPSDevice")
                .addWhere("?GPSDevice", "ontodevice:measures", "?Altitude")
                .addWhere("?Altitude", "rdf:type", "slma:Altitude")
                .addWhere("?Altitude", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
        return queryResult;
    }

    JSONArray getSpeedIRIArray(Node smartphoneIRI) {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRI, "saref:consistsOf", "?GPSDevice")
                .addWhere("?GPSDevice", "rdf:type", "ontodevice:GPSDevice")
                .addWhere("?GPSDevice", "ontodevice:measures", "?Speed")
                .addWhere("?Speed", "rdf:type", "om:Speed")
                .addWhere("?Speed", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
        return queryResult;
    }

    JSONArray getPointIRIArray(Node smartphoneIRI) {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addPrefix("sf", SF)
                .addWhere(smartphoneIRI, "saref:consistsOf", "?GPSDevice")
                .addWhere("?GPSDevice", "rdf:type", "ontodevice:GPSDevice")
                .addWhere("?GPSDevice", "ontodevice:hasGeoLocation", VAR_O)
                .addWhere(VAR_O, "rdf:type", "sf:Point");

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
        return queryResult;
    }

}
