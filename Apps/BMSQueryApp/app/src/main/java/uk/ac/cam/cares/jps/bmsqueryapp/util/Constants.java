package uk.ac.cam.cares.jps.bmsqueryapp.util;

import androidx.annotation.Nullable;

import java.util.HashMap;
import java.util.Map;

import okhttp3.HttpUrl;

public class Constants {
    private static final String HOST = "192.168.1.6";
    private static final String HOST_TEST = "10.0.2.2";

    public static String[] statusArray = {"SASH Opening", "Status", "Air Flow", "Damper State"};
    public static String[] statusArrayTemp = {"Graph", "Edit"};

    public static String[] uidArray = {"http://www.theworldavatar.com/BMS/CaresLab#V_SashOpeningOfWFH-04",
            "http://www.theworldavatar.com/BMS/CaresLab#V_StatusOfWFH-04",
            "http://www.theworldavatar.com/BMS/CaresLab#V_AirFlowofVAV-E/7-12",
            "http://www.theworldavatar.com/BMS/CaresLab#V_DamperStateOfVAV-E/7-12"};

    public static HttpUrl.Builder constructUrlBuilder(String path) {
        HttpUrl.Builder builder = new HttpUrl.Builder().scheme("http").host(HOST_TEST).port(3838);
        return builder.addPathSegments(path);
    }

    public static final Map<String, String> EQUIPMENT_TYPES = getEquipmentTypes();

    private static Map<String, String> getEquipmentTypes() {
        Map<String, String> equipmentTypes = new HashMap<>();
        equipmentTypes.put("WalkIn-FumeHood", "http://www.theworldavatar.com/BMS/CaresLab#WalkIn-FumeHood");
        return equipmentTypes;
    }

}
