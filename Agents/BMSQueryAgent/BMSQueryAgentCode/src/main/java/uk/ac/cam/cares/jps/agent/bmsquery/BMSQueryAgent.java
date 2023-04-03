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
import java.util.List;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

public class BMSQueryAgent {

    private static final Logger LOGGER = LogManager.getLogger(BMSQueryAgent.class);
    RemoteStoreClient rsClient;
    List<String> kgUrls;

    private final String BOT_STR =  "https://w3id.org/bot#";
    private final Prefix P_BOT = SparqlBuilder.prefix("bot", iri(BOT_STR));

    private final String BIM_STR = "http://www.theworldavatar.com/kg/ontobim/";
    private final Prefix P_BIM = SparqlBuilder.prefix("ontobim", iri(BIM_STR));

    public void setRSClient(RemoteStoreClient rsClient, List<String> kgUrls) {
        this.rsClient = rsClient;
        this.kgUrls = kgUrls;
    }

    public JSONObject queryAllZones() {
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
                    getAllFacilitiesInBuildings,
                    getBuildingsNames,
                    getFacilitiesTypes,
                    getFacilitiesNames,
                    getAllRoomsInFacilities,
                    getRoomsNames
                );

        JSONArray jsonResult;
        try {
            LOGGER.info("Sending federated request...");
            jsonResult = rsClient.executeFederatedQuery(kgUrls, query.getQueryString());
        } catch (Exception e) {
            LOGGER.error("Fail to run federated query to get everything in buildings");
            throw new JPSRuntimeException("Unable to get everything in buildings");
        }

        return parseTableToJSONObj(jsonResult);
    }

    JSONObject parseTableToJSONObj(JSONArray ja) {
        JSONObject result = new JSONObject();
        result.put("buildings", new HashMap<>());
        for (int i = 0; i < ja.length(); i++) {
            JSONObject originalJo = ja.getJSONObject(i);
            String building = originalJo.getString("building");
            String facility = originalJo.getString("facility");
            String room = originalJo.getString("room");

            JSONObject buildings = result.getJSONObject("buildings");
            if (!buildings.keySet().contains(building)) {
                buildings.put(building, new HashMap<>());
                buildings.getJSONObject(building).put("label", originalJo.getString("buildingLabel"));
                buildings.getJSONObject(building).put("facilities", new HashMap<>());
            }

            JSONObject facilities = buildings.getJSONObject(building).getJSONObject("facilities");
            if (!facilities.keySet().contains(facility)) {
                facilities.put(facility, new HashMap<>());
                facilities.getJSONObject(facility).put("label", originalJo.getString("facilityLabel"));
                facilities.getJSONObject(facility).put("rooms", new HashMap<>());
            }

            JSONObject rooms = facilities.getJSONObject(facility).getJSONObject("rooms");
            rooms.put(room, new HashMap<>());
            rooms.getJSONObject(room).put("label", originalJo.getString("roomLabel"));
        }
        return result;
    }

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
            LOGGER.info("Sending federated request with query: ");
            LOGGER.info(query.getQueryString());
            jsonResult = rsClient.executeFederatedQuery(kgUrls, query.getQueryString());
        } catch (Exception e) {
            LOGGER.error("Fail to run federated query to get equipment in the room: " + roomStr);
            throw new JPSRuntimeException("Unable to get equipment in the room: " + roomStr);
        }

        JSONObject result = new JSONObject();
        result.put("Equipments", parseLabel(jsonResult));
        LOGGER.info("Getting result:" + result);

        return result;
    }

    // CAUTION: the method assume @ is only for language specification in label
    private JSONArray parseLabel(JSONArray response) {
        for (int i = 0; i < response.length(); i++) {
            JSONObject jo = response.getJSONObject(i);
            String label = jo.getString("label");
            if (label.contains("@")) {
                label = label.split("@")[0];
            }
            if (label.charAt(0) == '"') {
                label = label.substring(1);
            }
            if (label.charAt(label.length()-1) == '"') {
                label = label.substring(0, label.length()-1);
            }
            jo.put("label", label);
        }
        return response;
    }

}
