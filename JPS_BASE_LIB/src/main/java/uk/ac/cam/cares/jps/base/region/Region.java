package uk.ac.cam.cares.jps.base.region;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.util.CRSTransformer;

/**
 * The purpose of this class is to store region specific parameters for dispersion modelling in one place.
 * In the future, these may be queried from the triplestore. 
 * There are currently 4 sets of scope, 2 for both Sg and HK.
 */
public class Region {
    // keys for JSON object
    public static String keyUppercorner = "uppercorner";
    public static String keyLowercorner = "lowercorner";
    public static String keyUpperx = "upperx";
    public static String keyUppery = "uppery";
    public static String keyLowerx = "lowerx";
    public static String keyLowery = "lowery";
    public static String keyRegion = "region";
    public static String keySrsname = "srsname";
    public static String keyAirStationIRI = "airStationIRI";
    public static String keyCity = "city";

    // City IRIs
    public static final String BERLIN_IRI = "http://dbpedia.org/resource/Berlin";
    public static final String THE_HAGUE_IRI = "http://dbpedia.org/resource/The_Hague";
    public static final String SINGAPORE_IRI = "http://dbpedia.org/resource/Singapore";
    public static final String HONG_KONG_IRI = "http://dbpedia.org/resource/Hong_Kong";
    public static final String PLYMOUTH_IRI = "http://dbpedia.org/resource/Plymouth";

    // City names
    private static final String Berlin = "Berlin";
    private static final String The_Hague = "The_Hague";
    private static final String Singapore = "Singapore";
    private static final String Hong_Kong = "Hong_Kong";
    private static final String Plymouth = "Plymouth";

    /**
     * Returns a JSONObject containing the scope coordinates and SRS name
     * Options: 1) Sg ADMS, 2) Sg Episode, 3) HK ADMS, 4) HK Episode
     */
    private static JSONObject getScope(int option) {
        String x_low = null, x_up = null , y_low = null, y_up = null, srsname = null;

        JSONObject joScope = new JSONObject();
        JSONObject joUppercorner = new JSONObject();
        JSONObject joLowercorner = new JSONObject();

        switch (option) {
        // Note: The coordinates here were obtained from CoordinationDataCollection (assumed to be the
        // most up-to-date scopes as they are the ones being simulated on Claudius periodically
            case 1: // Singapore ADMS
                x_low = "11560879.832"; 
                x_up  = "11564077.989";
                y_low = "140107.739"; 
                y_up  = "143305.896";
                srsname = CRSTransformer.EPSG_3857;
                break;

            case 2: // Singapore Episode
                x_low = "11552101.832"; 
                x_up  = "11572101.89";
                y_low = "131707.739"; 
                y_up  = "151860.32";
                srsname = CRSTransformer.EPSG_3857;
                break;

            case 3: // Hong Kong ADMS
                x_low = "12706653.262"; 
                x_up  = "12711879.81";
                y_low = "2545200.172"; 
                y_up  = "2550426.72";
                srsname = CRSTransformer.EPSG_3857;
                break;

            case 4: // Hong Kong Episode
                x_low = "12694101.21"; 
                x_up  = "12720578.56";
                y_low = "2534900.06"; 
                y_up  = "2562555.26";
                srsname = CRSTransformer.EPSG_3857;
                break;
                
            case 5: // Plymouth Episode
            	x_low = "237044.13";
            	x_up = "257044.13";
                y_low = "44991.65";
                y_up = "64991.65";
                srsname = "EPSG:27700";
                break;
        }
        joUppercorner.put(keyUpperx, x_up);
        joUppercorner.put(keyUppery, y_up);
        joLowercorner.put(keyLowerx, x_low);
        joLowercorner.put(keyLowery, y_low);

        joScope.put(keySrsname, srsname);
        joScope.put(keyLowercorner, joLowercorner);
        joScope.put(keyUppercorner, joUppercorner);

        return joScope;
    }

    /**
     * Adds region to the JSON object received
     * Options: 1) Sg ADMS, 2) Sg Episode, 3) HK ADMS, 4) HK Episode, 5) Plymouth Episode
     */
    public static void putRegion(JSONObject jo, int option) {
        jo.put(keyRegion, getScope(option));
    }

    /**
     * Adds region and air quality station IRI to the JSON object received
     * The air quality stations were created manually for each option
     * Options: 1) Sg ADMS, 2) Sg Episode, 3) HK ADMS, 4) HK Episode
     */
    public static void putRegionAndStation(JSONObject jo, int option) {
        jo.put(keyRegion, getScope(option));

        switch (option) {
            case 1: // Singapore ADMS
                jo.put(keyAirStationIRI, "http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001");
                jo.put(keyCity, SINGAPORE_IRI);
                break;
            case 2: // Singapore Episode
                jo.put(keyAirStationIRI, "http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-002.owl#AirQualityStation-002");
                jo.put(keyCity, SINGAPORE_IRI);
                break;
            case 3: // Hong Kong ADMS
                jo.put(keyAirStationIRI, "http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-001.owl#AirQualityStation-001");
                jo.put(keyCity, HONG_KONG_IRI);
                break;
            case 4: // Hong Kong Episode
                jo.put(keyAirStationIRI, "http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-002.owl#AirQualityStation-002");
                jo.put(keyCity, HONG_KONG_IRI);
                break;
            case 5: // Plymouth Episode
            	// dummy air quality station (temporary)
            	jo.put(keyAirStationIRI, "http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-002.owl#AirQualityStation-002");
            	jo.put(keyCity, PLYMOUTH_IRI);
            	break;
        }
    }

    /**
     * Different combinations of city and model (Episode/ADMS) require different CRS 
     */
    public static String getTargetCRSName(String agentIRI, String cityIRI) {
        String targetCRSName = null;

        if (cityIRI.contains(Berlin)) {
            targetCRSName = CRSTransformer.EPSG_25833;
        }
        else if (cityIRI.contains(The_Hague)) {
            targetCRSName = CRSTransformer.EPSG_28992;
        }
        else if (cityIRI.contains(Singapore)) {
            if (agentIRI.contains("ADMS")) {
                targetCRSName = CRSTransformer.EPSG_3414;
            }
            else {// Episode
                targetCRSName = CRSTransformer.EPSG_32648;
            }
        }
        else if (cityIRI.contains(Hong_Kong)) {
            if (agentIRI.contains("ADMS")) {
                targetCRSName = CRSTransformer.EPSG_2326;
            }
            else {// Episode
                targetCRSName = CRSTransformer.EPSG_32650;
            }
        }
        else if (cityIRI.contains(Plymouth)) {
        	targetCRSName = "EPSG:32630";
        }
        return targetCRSName;
    }

    /**
     * Get SRTM file names required for Episode topology preprocessor
     * Ideally these files should be stored in some kind of database
     * Note that the code is currently hard coded to take in a maximum of 2 files
     * Shortcut fix to tidy up Episode agent
     */
    public static List<String> getSRTM(String cityIRI) {
        List<String> srtm = new ArrayList<String>();
        if (cityIRI.contains(Singapore)) {
            srtm.add("N01E103");
            srtm.add("N01E104");
        } else if (cityIRI.contains(Hong_Kong)) {
            srtm.add("N22E114");
        } else if (cityIRI.contains(Plymouth)) {
        	srtm.add("N50W005");
        }
        return srtm;
    }
}
