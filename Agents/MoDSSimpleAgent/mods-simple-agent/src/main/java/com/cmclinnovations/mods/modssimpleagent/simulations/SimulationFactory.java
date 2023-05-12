package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.IOException;

import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.utils.SimulationSaver;

public class SimulationFactory {

    private SimulationFactory() {
    }

    public static Simulation createSimulation(Request request, BackendInputFile inputFile, MoDSBackend modsBackend,
            InputMetaData inputMetaData, SimulationSaver simulationSaver) throws IOException {

        String simulationType = request.simulationType();
        switch (simulationType) {
            case "MOO":
                return new MOO(request, inputFile, modsBackend, inputMetaData, simulationSaver);
            case "HDMR":
                return new HDMR(request, inputFile, modsBackend, inputMetaData, simulationSaver);
            case "MOOonly":
                return new MOOonly(request, inputFile, modsBackend, inputMetaData, simulationSaver);
            case "MCDM":
                return new MCDM(request, inputFile, modsBackend, inputMetaData, simulationSaver);
            case "Evaluate":
                return new Evaluate(request, inputFile, modsBackend, inputMetaData, simulationSaver);
            case "Sensitivity":
                return new Sensitivity(request, inputFile, modsBackend, inputMetaData, simulationSaver);
            default:
                throw new IllegalArgumentException("Unknown simulation type requested '" + simulationType + "'.");
        }
    }
}
