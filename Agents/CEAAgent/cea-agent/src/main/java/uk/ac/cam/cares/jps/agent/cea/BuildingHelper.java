package uk.ac.cam.cares.jps.agent.cea;

import java.util.Arrays;

public class BuildingHelper {
    public static final String BUILDING = "building";

    /**
     * Gets namespace uri from input city object uri.
     * @param uriString city object id
     * @return Object's namespace
     */
    public static String getNamespace(String uriString) {
        String[] splitUri = uriString.split("/");
        return String.join("/", Arrays.copyOfRange(splitUri, 0, splitUri.length - 2))+"/";
    }
    
    /**
     * Creates graph uri from input city object uri and graph name tag
     * @param uriString city object id
     * @param graph name tag of graph wanted
     * @return Requested graph with correct namespace
     */
    public static String getGraph(String uriString, String graph) {
        String namespace = getNamespace(uriString);
        return namespace + graph + "/";
    }
    
    /**
     * Gets building uri from input city object uri
     * @param uriString city object id
     * @return Building uri
     */
    public static String getBuildingUri(String uriString) {
        return getGraph(uriString, BUILDING)+getUUID(uriString)+"/";
    }

    /**
     * Gets UUID from input city object uri
     * @param uriString city object id
     * @return Requested UUID
     */
    public static String getUUID(String uriString) {
        String[] splitUri = uriString.split("/");
        return splitUri[splitUri.length-1];
    }

    /**
     * Gets building graph uri from input city object id
     * @param uriString city object id
     * @return building graph uri
     */
    public static String getBuildingGraph(String uriString) {
        return getGraph(uriString, BUILDING);
    }
}
