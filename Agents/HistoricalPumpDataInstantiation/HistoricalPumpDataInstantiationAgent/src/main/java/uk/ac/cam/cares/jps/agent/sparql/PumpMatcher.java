package uk.ac.cam.cares.jps.agent.sparql;

import java.util.HashMap;
import java.util.Map;

/**
 * Provides a map of each pump to their IRIs.
 *
 * @author qhouyee
 */
class PumpMatcher {
    private static final String pumpStationIRI = "http://www.theworldavatar.com/kg/ps/pumpstation-";

    /**
     * Generate mappings to map each pump to their existing IRI.
     */
    protected static Map<String, String> genMappings() {
        Map<String, String> pumpMap = new HashMap<>();
        // Pumps with their IRI present
        pumpMap.put("RubLandauerStrasseparkwaldsiedelung", pumpStationIRI + "1.05");
        pumpMap.put("PwSwRodalberStrasse", pumpStationIRI + "1.14");
        pumpMap.put("PwSwmassachu", pumpStationIRI + "1.17");
        pumpMap.put("PwMwFehrbach", pumpStationIRI + "1.29");
        pumpMap.put("PwWindsbergHochwaldstr", pumpStationIRI + "1.33");
        pumpMap.put("PstGersbachwindsbergerstr", pumpStationIRI + "1.38");
        pumpMap.put("PwSwerlenteichRheinstr", pumpStationIRI + "1.47");
        pumpMap.put("PwMwamHollerstock", pumpStationIRI + "1.53");
        pumpMap.put("PwMwamStockwald", pumpStationIRI + "1.56");
        pumpMap.put("PwMwadammullerstr", pumpStationIRI + "1.60");
        pumpMap.put("PwSwsteinigerBuhl", pumpStationIRI + "2.15");
        pumpMap.put("PwSwsommerweg", pumpStationIRI + "2.17");
        pumpMap.put("PwSwalteLandstrasse", pumpStationIRI + "2.18");
        pumpMap.put("PwMwsengelsberg", pumpStationIRI + "2.19");
        pumpMap.put("PwSwhorbacherWeg", pumpStationIRI + "2.29");
        pumpMap.put("PwSwanDerSteige", pumpStationIRI + "2.30");
        pumpMap.put("RubBottenbacherstr", pumpStationIRI + "116_126_HEB");
        return pumpMap;
    }
}
