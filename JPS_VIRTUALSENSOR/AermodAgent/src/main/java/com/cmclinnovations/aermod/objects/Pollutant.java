package com.cmclinnovations.aermod.objects;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.cmclinnovations.aermod.QueryClient;

public class Pollutant {
    public enum PollutantType {
        NO_X, UHC, CO, CO2, SO2, PM10, PM2_5
    }

    // used to create folders within the aermod simulation directory and filter for
    // contours table
    private static Map<PollutantType, String> idToLabelMap = new EnumMap<>(PollutantType.class);
    static {
        idToLabelMap.put(PollutantType.NO_X, "NOx");
        idToLabelMap.put(PollutantType.UHC, "uHC");
        idToLabelMap.put(PollutantType.CO, "CO");
        idToLabelMap.put(PollutantType.CO2, "CO2");
        idToLabelMap.put(PollutantType.SO2, "SO2");
        idToLabelMap.put(PollutantType.PM10, "PM10");
        idToLabelMap.put(PollutantType.PM2_5, "PM2_5");
    }

    private static Map<String, PollutantType> iriToTypeMap = new HashMap<>();
    static {
        iriToTypeMap.put(QueryClient.NO_X, PollutantType.NO_X);
        iriToTypeMap.put(QueryClient.UHC, PollutantType.UHC);
        iriToTypeMap.put(QueryClient.CO, PollutantType.CO);
        iriToTypeMap.put(QueryClient.CO2, PollutantType.CO2);
        iriToTypeMap.put(QueryClient.SO2, PollutantType.SO2);
        iriToTypeMap.put(QueryClient.PM10, PollutantType.PM10);
        iriToTypeMap.put(QueryClient.PM25, PollutantType.PM2_5);
    }

    private static EnumMap<PollutantType, String> typeToIriMap = new EnumMap<>(PollutantType.class);
    static {
        typeToIriMap.put(PollutantType.NO_X, QueryClient.NO_X);
        typeToIriMap.put(PollutantType.UHC, QueryClient.UHC);
        typeToIriMap.put(PollutantType.CO, QueryClient.CO);
        typeToIriMap.put(PollutantType.CO2, QueryClient.CO2);
        typeToIriMap.put(PollutantType.SO2, QueryClient.SO2);
        typeToIriMap.put(PollutantType.PM10, QueryClient.PM10);
        typeToIriMap.put(PollutantType.PM2_5, QueryClient.PM25);
    }

    public static String getPollutantLabel(PollutantType type) {
        return idToLabelMap.get(type);
    }

    public static PollutantType getPollutantType(String iri) {
        return iriToTypeMap.get(iri);
    }

    public static String getPollutantIri(PollutantType pollutantType) {
        return typeToIriMap.get(pollutantType);
    }

    public static List<PollutantType> getPollutantList() {
        return List.of(PollutantType.NO_X, PollutantType.UHC, PollutantType.CO, PollutantType.CO2, PollutantType.SO2,
                PollutantType.PM10, PollutantType.PM2_5);
    }
}
