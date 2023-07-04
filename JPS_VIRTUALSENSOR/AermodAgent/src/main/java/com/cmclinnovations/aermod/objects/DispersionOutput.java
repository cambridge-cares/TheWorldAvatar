package com.cmclinnovations.aermod.objects;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.cmclinnovations.aermod.objects.Pollutant.PollutantType;

public class DispersionOutput {
    Map<PollutantType, String> pollutantToDispMatrixMap;
    // may want to use height IRI instead of double in case of rounding errors
    Map<PollutantType, Map<Double, String>> pollutantToDispLayerMap;
    Map<PollutantType, String> pollutantToRasterMap;
    List<PollutantType> pollutantTypeList;

    public DispersionOutput() {
        pollutantToDispMatrixMap = new EnumMap<>(PollutantType.class);
        pollutantToDispLayerMap = new EnumMap<>(PollutantType.class);
        pollutantToRasterMap = new EnumMap<>(PollutantType.class);
        pollutantTypeList = new ArrayList<>();
    }

    public void addDispMatrix(PollutantType pollutantType, String dispMatrix) {
        pollutantToDispMatrixMap.put(pollutantType, dispMatrix);
        if (!pollutantTypeList.contains(pollutantType)) {
            pollutantTypeList.add(pollutantType);
        }
    }

    public void addDispLayer(PollutantType pollutantType, double height, String dispLayer) {
        pollutantToDispLayerMap.computeIfAbsent(pollutantType, k -> new HashMap<>());
        pollutantToDispLayerMap.get(pollutantType).put(height, dispLayer);
        if (!pollutantTypeList.contains(pollutantType)) {
            pollutantTypeList.add(pollutantType);
        }
    }

    public void addDispRaster(PollutantType pollutantType, String raster) {
        pollutantToRasterMap.put(pollutantType, raster);
        if (!pollutantTypeList.contains(pollutantType)) {
            pollutantTypeList.add(pollutantType);
        }
    }

    public boolean checkDataHasSamePollutants() {
        return ((pollutantToDispMatrixMap.keySet().size() == pollutantToDispLayerMap.keySet().size())
                && (pollutantToDispMatrixMap.keySet().size() == pollutantToRasterMap.keySet().size()));
    }

    public String getDispMatrix(PollutantType pollutantType) {
        return pollutantToDispMatrixMap.get(pollutantType);
    }

    public String getDispLayer(PollutantType pollutantType, double height) {
        return pollutantToDispLayerMap.get(pollutantType).get(height);
    }

    public List<String> getAllDispLayer() {
        List<String> dispLayers = new ArrayList<>();
        pollutantToDispLayerMap.keySet()
                .forEach(pollutant -> dispLayers.addAll(pollutantToDispLayerMap.get(pollutant).values()));
        return dispLayers;
    }

    public String getDispRaster(PollutantType pollutantType) {
        return pollutantToRasterMap.get(pollutantType);
    }

    public boolean hasPollutant(PollutantType pollutantType) {
        return pollutantTypeList.contains(pollutantType);
    }
}
