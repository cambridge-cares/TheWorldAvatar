package uk.ac.cam.cares.jps.base.region;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class Region {
    /**
     * The purpose of this class is to store region specific parameters for dispersion modelling in one place.
     * In the future, these may be queried from the triplestore. 
     * There are currently 4 sets of scope, 2 for both Sg and HK.
     */

    // keys for JSON object
    public static String keyUppercorner = "uppercorner";
    public static String keyLowercorner = "lowercorner";
    public static String keyUpperx = "upperx";
    public static String keyUppery = "uppery";
    public static String keyLowerx = "lowerx";
    public static String keyLowery = "lowery";
    public static String keyRegion = "region";
    public static String keySrsname = "srsname";

    // City IRIs
    public static final String BERLIN_IRI = "http://dbpedia.org/resource/Berlin";
    public static final String THE_HAGUE_IRI = "http://dbpedia.org/resource/The_Hague";
    public static final String SINGAPORE_IRI = "http://dbpedia.org/resource/Singapore";
    public static final String HONG_KONG_IRI = "http://dbpedia.org/resource/Hong_Kong";

    private JSONObject getScope(int option) {
        /**
         * Returns a JSONObject containing the scope coordinates and SRS name
         * Options: 1) Sg ADMS, 2) Sg Episode, 3) HK ADMS, 4) HK Episode
         */
        String x_low = null, x_up = null , y_low = null, y_up = null;

        JSONObject joScope = new JSONObject();
        JSONObject joUppercorner = new JSONObject();
        JSONObject joLowercorner = new JSONObject();

        switch (option) {
            case 1: // Singapore ADMS
                x_low = "11560879.832"; 
                x_up  = "11564077.989";
                y_low = "140107.739"; 
                y_up  = "143305.896";

            case 2: // Singapore Episode
                x_low = "11552101.832"; 
                x_up  = "11572101.89";
                y_low = "131707.739"; 
                y_up  = "151860.32";

            case 3: // Hong Kong ADMS
                x_low = "12706653.262"; 
                x_up  = "12711879.81";
                y_low = "2545200.172"; 
                y_up  = "2550426.72";

            case 4: // Hong Kong Episode
                x_low = "12694101.21"; 
                x_up  = "12720578.56";
                y_low = "2534900.06"; 
                y_up  = "2562555.26";
        }
        joUppercorner.put(keyUpperx, x_up);
        joUppercorner.put(keyUppery, y_up);
        joLowercorner.put(keyLowerx, x_low);
        joLowercorner.put(keyLowery, y_low);

        joScope.put(keySrsname, CRSTransformer.EPSG_3857);
        joScope.put(keyLowercorner, joLowercorner);
        joScope.put(keyUppercorner, joUppercorner);

        return joScope;
    }

    public void putRegion(JSONObject jo, int option) {
        /**
         * Adds region JSON object to the JSON object received
         * Options: 1) Sg ADMS, 2) Sg Episode, 3) HK ADMS, 4) HK Episode
         */
        jo.put(keyRegion, getScope(option));
    }

    public static String getTargetCRSName(String agentIRI, String cityIRI) {
        /**
         * Different combinations of city and model (Episode/ADMS) require different CRS 
         */
        String targetCRSName = null;

        if (cityIRI.equalsIgnoreCase(BERLIN_IRI)) {
            targetCRSName = CRSTransformer.EPSG_25833;
        }
        else if (cityIRI.equalsIgnoreCase(THE_HAGUE_IRI)) {
            targetCRSName = CRSTransformer.EPSG_28992;
        }
        else if (cityIRI.equalsIgnoreCase(SINGAPORE_IRI)) {
            if (agentIRI.contains("ADMS")) {
                targetCRSName = CRSTransformer.EPSG_3414;
            }
            else {// Episode
                targetCRSName = CRSTransformer.EPSG_32648;
            }
        }
        else if (cityIRI.equalsIgnoreCase(HONG_KONG_IRI)) {
            if (agentIRI.contains("ADMS")) {
                targetCRSName = CRSTransformer.EPSG_2326;
            }
            else {// Episode
                targetCRSName = CRSTransformer.EPSG_32650;
            }
        }
        return targetCRSName;
    }
}
