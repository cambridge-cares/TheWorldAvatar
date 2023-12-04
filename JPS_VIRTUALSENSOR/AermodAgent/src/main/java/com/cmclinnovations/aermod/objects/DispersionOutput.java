package com.cmclinnovations.aermod.objects;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import com.cmclinnovations.aermod.objects.Pollutant.PollutantType;

public class DispersionOutput {
    Map<PollutantType, String> pollutantToDispMatrixMap;
    Map<PollutantType, String> pollutantToRasterMap;
    Map<PollutantType, String> pollutantToColourBarMap;
    List<PollutantType> pollutantTypeList;

    public DispersionOutput() {
        pollutantToDispMatrixMap = new EnumMap<>(PollutantType.class);
        pollutantToRasterMap = new EnumMap<>(PollutantType.class);
        pollutantToColourBarMap = new EnumMap<>(PollutantType.class);
        pollutantTypeList = new ArrayList<>();
    }

    public void addDispMatrix(PollutantType pollutantType, String dispMatrix) {
        pollutantToDispMatrixMap.put(pollutantType, dispMatrix);
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

    public void addColourBar(PollutantType pollutantType, String colourBar) {
        pollutantToColourBarMap.put(pollutantType, colourBar);
        if (!pollutantTypeList.contains(pollutantType)) {
            pollutantTypeList.add(pollutantType);
        }
    }

    public String getDispMatrix(PollutantType pollutantType) {
        return pollutantToDispMatrixMap.get(pollutantType);
    }

    public String getDispRaster(PollutantType pollutantType) {
        return pollutantToRasterMap.get(pollutantType);
    }

    public String getColourBar(PollutantType pollutantType) {
        return pollutantToColourBarMap.get(pollutantType);
    }

    public boolean hasPollutant(PollutantType pollutantType) {
        return pollutantTypeList.contains(pollutantType);
    }
}
