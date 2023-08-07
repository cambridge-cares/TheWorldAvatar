package uk.ac.cam.cares.jps.agent.cea.utils.uri;

import java.util.Arrays;

public class BuildingURIHelper {
    /**
     * Gets namespace uri from input city object uri.
     * @param uriString city object id
     * @return Object's namespace
     */
    public static String getNamespace(String uriString) {
        String[] splitUri = uriString.split("/");
        return String.join("/", Arrays.copyOfRange(splitUri, 0, splitUri.length - 2))+"/";
    }
}
