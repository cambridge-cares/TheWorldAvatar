package uk.ac.cam.cares.jps.agent.bmsquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.sparqlbuilder.constraint.propertypath.PropertyPath;
import org.eclipse.rdf4j.sparqlbuilder.constraint.propertypath.builder.PropertyPathBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.HashMap;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

/**
 * This class contains the agent's main functionalities.
 *
 * @author sandradeng20
 */
public class BMSQueryAgent {

    private static final Logger LOGGER = LogManager.getLogger(BMSQueryAgent.class);
    RemoteStoreClient labRsClient;
    RemoteStoreClient officeRsClient;

    private final String BOT_STR = "https://w3id.org/bot#";
    private final Prefix P_BOT = SparqlBuilder.prefix("bot", iri(BOT_STR));

    private final String BIM_STR = "https://www.theworldavatar.com/kg/ontobim/";
    private final Prefix P_BIM = SparqlBuilder.prefix("ontobim", iri(BIM_STR));

    /**
     * Setter for RemoteStoreClient and Knowledge graph namespace urls
     * @param labRsClient RemoteStoreClient instance for lab namespace
     * @param officeRsClient RemoteStoreClient instance for office namespace
     */
    public void setRSClient(RemoteStoreClient labRsClient, RemoteStoreClient officeRsClient) {
        this.labRsClient = labRsClient;
        this.officeRsClient = officeRsClient;
    }

    /**
     * Query buildings, facilities, rooms from lab namespace and keep the hierarchy of zones.
     * @return JSONObject with zone hierarchy
     */
    public JSONObject queryLabZones() {
        String queryString  = getZoneQueryString();

        LOGGER.info(queryString);
        JSONArray jsonResult;
        try {
            LOGGER.info("Sending request...");
            jsonResult = labRsClient.executeQuery(queryString);
        } catch (Exception e) {
            LOGGER.error("Fail to run query to get everything in buildings");
            throw new JPSRuntimeException("Unable to get everything in buildings");
        }

        return parseTableToJSONObj(jsonResult);
    }

    /**
     * Query buildings, facilities, rooms from office namespace and keep the hierarchy of zones.
     * @return JSONObject with zone hierarchy
     */
    public JSONObject queryOfficeZones() {
        String queryString  = getZoneQueryString();

        LOGGER.info(queryString);
        JSONArray jsonResult;
        try {
            LOGGER.info("Sending request...");
            jsonResult = officeRsClient.executeQuery(queryString);
        } catch (Exception e) {
            LOGGER.error("Fail to run query to get everything in buildings");
            throw new JPSRuntimeException("Unable to get everything in buildings");
        }

        return parseTableToJSONObj(jsonResult);
    }

    public JSONObject queryAllZones() {
        JSONObject officeZones = queryOfficeZones();
        JSONObject labZones = queryLabZones();

        JSONObject buildingJo = officeZones.getJSONObject("buildings");
        for (String key : labZones.getJSONObject("buildings").keySet()) {
            buildingJo.put(key, labZones.getJSONObject("buildings").getJSONObject(key));
        }
        return officeZones;
    }

    private String getZoneQueryString() {
        Variable building = SparqlBuilder.var("building");
        Variable facility = SparqlBuilder.var("facility");
        Variable buildingLabel = SparqlBuilder.var("buildingLabel");
        TriplePattern getAllBuildings = GraphPatterns.tp(building, RDF.TYPE, P_BOT.iri("Building"));
        TriplePattern getAllFacilitiesInBuildings = GraphPatterns.tp(building, P_BIM.iri("hasFacility"), facility);
        PropertyPath ifcLabel = PropertyPathBuilder.of(P_BIM.iri("hasIfcRepresentation")).then(RDFS.LABEL).build();
        TriplePattern getBuildingsNames = GraphPatterns.tp(building, ifcLabel, buildingLabel);

        Variable facilityType = SparqlBuilder.var("facilityType");
        Variable room = SparqlBuilder.var("room");
        Variable facilityLabel = SparqlBuilder.var("facilityLabel");
        TriplePattern getFacilitiesTypes = GraphPatterns.tp(facility, RDF.TYPE, facilityType);
        TriplePattern getFacilitiesNames = GraphPatterns.tp(facility, RDFS.LABEL, facilityLabel);
        TriplePattern getAllRoomsInFacilities = GraphPatterns.tp(facility, P_BIM.iri("hasRoom"), room);

        Variable roomLabel = SparqlBuilder.var("roomLabel");
        TriplePattern getRoomsNames = GraphPatterns.tp(room, ifcLabel, roomLabel);

        // allow optional in (facility and room), optional in room
        SelectQuery query = Queries.SELECT();
        query.prefix(P_BIM, P_BOT)
                .distinct()
                .select(building,
                        facility,
                        buildingLabel,
                        facilityType,
                        room,
                        facilityLabel,
                        roomLabel)
                .where(getAllBuildings,
                        getBuildingsNames,
                        GraphPatterns.optional(getAllFacilitiesInBuildings, getFacilitiesTypes, getFacilitiesNames,
                                GraphPatterns.optional(getAllRoomsInFacilities, getRoomsNames))
                );
        return query.getQueryString();
    }

    /**
     * Convert table like JSONArray data to zone hierarchy JSONObject
     * @param ja JSONArray with table structure
     * @return JSONObject with zone hierarchy
     */
    private JSONObject parseTableToJSONObj(JSONArray ja) {
        JSONObject result = new JSONObject();
        result.put("buildings", new HashMap<>());
        for (int i = 0; i < ja.length(); i++) {
            JSONObject originalJo = ja.getJSONObject(i);

            String building = originalJo.getString("building");
            JSONObject buildings = result.getJSONObject("buildings");
            if (!buildings.keySet().contains(building)) {
                buildings.put(building, new HashMap<>());
                buildings.getJSONObject(building).put("label", originalJo.getString("buildingLabel"));
                buildings.getJSONObject(building).put("facilities", new HashMap<>());
            }

            if (!originalJo.has("facility")) {
                continue;
            }
            String facility = originalJo.getString("facility");
            JSONObject facilities = buildings.getJSONObject(building).getJSONObject("facilities");
            if (!facilities.keySet().contains(facility)) {
                facilities.put(facility, new HashMap<>());
                facilities.getJSONObject(facility).put("label", originalJo.getString("facilityLabel"));
                facilities.getJSONObject(facility).put("rooms", new HashMap<>());
            }

            if (!originalJo.has("room")) {
                continue;
            }
            String room = originalJo.getString("room");
            JSONObject rooms = facilities.getJSONObject(facility).getJSONObject("rooms");
            rooms.put(room, new HashMap<>());
            rooms.getJSONObject(room).put("label", originalJo.getString("roomLabel"));
        }
        return result;
    }

    /**
     * Get all equipment instances in the given room
     * @param roomStr Room IRI String
     * @return all equipment instances in the room
     */
    public JSONObject queryEquipmentInstances(String roomStr) {
        SelectQuery query = Queries.SELECT();
        Variable element = SparqlBuilder.var("iri");
        Variable elementType = SparqlBuilder.var("type");
        Variable elementLabel = SparqlBuilder.var("label");

        Iri roomIri = iri(roomStr);

        TriplePattern getElementsInRoom = GraphPatterns.tp(roomIri, P_BOT.iri("containsElement"), element);
        TriplePattern getElementsTypes = GraphPatterns.tp(element, RDF.TYPE, elementType);
        TriplePattern getElementsLabels = GraphPatterns.tp(element, RDFS.LABEL, elementLabel);

        query.prefix(P_BOT).distinct().select(element, elementLabel, elementType).where(getElementsInRoom, getElementsTypes, getElementsLabels);

        JSONArray jsonResult;
        try {
            LOGGER.info("Sending request with query: ");
            LOGGER.info(query.getQueryString());
            jsonResult = labRsClient.executeQuery(query.getQueryString());
        } catch (Exception e) {
            LOGGER.error("Fail to run query to get equipment in the room: " + roomStr);
            throw new JPSRuntimeException("Unable to get equipment in the room: " + roomStr);
        }

        JSONObject result = new JSONObject();
        result.put("equipment", parseLabel(jsonResult));
        LOGGER.info("Getting result:" + result);

        return result;
    }

    /**
     * Parse the name of equipment instance
     * @param response equipment instances
     * @return equipment instances with more human-readable instance name
     */
    private JSONArray parseLabel(JSONArray response) {
        // CAUTION: the method assume @ is only for language specification in label
        for (int i = 0; i < response.length(); i++) {
            JSONObject jo = response.getJSONObject(i);
            String label = jo.getString("label");
            if (label.contains("@")) {
                label = label.split("@")[0];
            }
            if (label.charAt(0) == '"') {
                label = label.substring(1);
            }
            if (label.charAt(label.length() - 1) == '"') {
                label = label.substring(0, label.length() - 1);
            }
            jo.put("label", label);
        }
        return response;
    }

}
