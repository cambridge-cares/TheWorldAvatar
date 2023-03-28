package uk.ac.cam.cares.jps.bmsqueryapp.util;

import androidx.annotation.Nullable;

import java.util.HashMap;
import java.util.Map;

import okhttp3.HttpUrl;

public class Constants {
    private static final String HOST = "192.168.1.6";
    private static final String HOST_TEST = "10.0.2.2";
    public static String[] statusArrayTemp = {"Graph", "Edit"};

    public static HttpUrl.Builder constructUrlBuilder(String path) {
        HttpUrl.Builder builder = new HttpUrl.Builder().scheme("http").host(HOST_TEST).port(3838);
        return builder.addPathSegments(path);
    }

    public static final Map<String, String> EQUIPMENT_TYPES = getEquipmentTypes();

    // TODO: the equipmentTypes uri should be from kg as well
    private static Map<String, String> getEquipmentTypes() {
        Map<String, String> equipmentTypes = new HashMap<>();
        equipmentTypes.put("WalkIn-FumeHood", "https://www.theworldavatar.com/kg/ontobms/WalkInFumeHood");
        equipmentTypes.put("Cooling Fan", "https://saref.etsi.org/core/HVAC");
        return equipmentTypes;
    }

}
