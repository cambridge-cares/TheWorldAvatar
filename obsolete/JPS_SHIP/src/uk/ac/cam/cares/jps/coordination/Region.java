package uk.ac.cam.cares.jps.coordination;

import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public class Region {
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
    public static final String BERLIN_IRI = "http://dbpedia.org/resource/Berlin";
    public static final String THE_HAGUE_IRI = "http://dbpedia.org/resource/The_Hague";
    public static final String SINGAPORE_IRI = "http://dbpedia.org/resource/Singapore";
    public static final String HONG_KONG_IRI = "http://dbpedia.org/resource/Hong_Kong";
    public static final String PLYMOUTH_IRI = "http://dbpedia.org/resource/Plymouth";
    private static final String Berlin = "Berlin";
    private static final String The_Hague = "The_Hague";
    private static final String Singapore = "Singapore";
    private static final String Hong_Kong = "Hong_Kong";
    private static final String Plymouth = "Plymouth";

    public Region() {
    }

    private static JSONObject getScope(int option) {
        String x_low = null;
        String x_up = null;
        String y_low = null;
        String y_up = null;
        String srsname = null;
        JSONObject joScope = new JSONObject();
        JSONObject joUppercorner = new JSONObject();
        JSONObject joLowercorner = new JSONObject();
        switch(option) {
            case 1:
                x_low = "11560879.832";
                x_up = "11564077.989";
                y_low = "140107.739";
                y_up = "143305.896";
                srsname = "EPSG:3857";
                break;
            case 2:
                x_low = "11552101.832";
                x_up = "11572101.89";
                y_low = "131707.739";
                y_up = "151860.32";
                srsname = "EPSG:3857";
                break;
            case 3:
                x_low = "12706653.262";
                x_up = "12711879.81";
                y_low = "2545200.172";
                y_up = "2550426.72";
                srsname = "EPSG:3857";
                break;
            case 4:
                x_low = "12694101.21";
                x_up = "12720578.56";
                y_low = "2534900.06";
                y_up = "2562555.26";
                srsname = "EPSG:3857";
                break;
            case 5:
                x_low = "237044.13";
                x_up = "257044.13";
                y_low = "44991.65";
                y_up = "64991.65";
                srsname = "EPSG:27700";
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

    public static void putRegion(JSONObject jo, int option) {
        jo.put(keyRegion, getScope(option));
    }

    public static void putRegionAndStation(JSONObject jo, int option) {
        jo.put(keyRegion, getScope(option));
        switch(option) {
            case 1:
                jo.put(keyAirStationIRI, "http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-001.owl#AirQualityStation-001");
                jo.put(keyCity, "http://dbpedia.org/resource/Singapore");
                break;
            case 2:
                jo.put(keyAirStationIRI, "http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-002.owl#AirQualityStation-002");
                jo.put(keyCity, "http://dbpedia.org/resource/Singapore");
                break;
            case 3:
                jo.put(keyAirStationIRI, "http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-001.owl#AirQualityStation-001");
                jo.put(keyCity, "http://dbpedia.org/resource/Hong_Kong");
                break;
            case 4:
                jo.put(keyAirStationIRI, "http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-002.owl#AirQualityStation-002");
                jo.put(keyCity, "http://dbpedia.org/resource/Hong_Kong");
                break;
            case 5:
                jo.put(keyAirStationIRI, "http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-002.owl#AirQualityStation-002");
                jo.put(keyCity, "http://dbpedia.org/resource/Plymouth");
        }

    }

    public static String getTargetCRSName(String agentIRI, String cityIRI) {
        String targetCRSName = null;
        if (cityIRI.contains("Berlin")) {
            targetCRSName = "EPSG:25833";
        } else if (cityIRI.contains("The_Hague")) {
            targetCRSName = "EPSG:28992";
        } else if (cityIRI.contains("Singapore")) {
            if (agentIRI.contains("ADMS")) {
                targetCRSName = "EPSG:3414";
            } else {
                targetCRSName = "EPSG:32648";
            }
        } else if (cityIRI.contains("Hong_Kong")) {
            if (agentIRI.contains("ADMS")) {
                targetCRSName = "EPSG:2326";
            } else {
                targetCRSName = "EPSG:32650";
            }
        } else if (cityIRI.contains("Plymouth")) {
            targetCRSName = "EPSG:32630";
        }

        return targetCRSName;
    }

    public static List<String> getSRTM(String cityIRI) {
        List<String> srtm = new ArrayList();
        if (cityIRI.contains("Singapore")) {
            srtm.add("N01E103");
            srtm.add("N01E104");
        } else if (cityIRI.contains("Hong_Kong")) {
            srtm.add("N22E114");
        } else if (cityIRI.contains("Plymouth")) {
            srtm.add("N50W005");
        }

        return srtm;
    }
}
