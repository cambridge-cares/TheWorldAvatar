package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.IOException;

import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;

public class SimulationFactory {

    private SimulationFactory() {
    }

    public static Simulation createSimulation(Request request, BackendInputFile inputFile, MoDSBackend modsBackend,
            InputMetaData inputMetaData) throws IOException {

        String simulationType = request.simulationType();
        switch (simulationType) {
            case "MOO":
                return new MOO(request, inputFile, modsBackend, inputMetaData);
            case "HDMR":
                return new HDMR(request, inputFile, modsBackend, inputMetaData);
            case "MOOonly":
                return new MOOonly(request, inputFile, modsBackend, inputMetaData);
            case "MCDM":
                return new MCDM(request, inputFile, modsBackend, inputMetaData);
            case "Evaluate":
                return new Evaluate(request, inputFile, modsBackend, inputMetaData);
            case "Sensitivity":
                return new Sensitivity(request, inputFile, modsBackend, inputMetaData);
            default:
                throw new IllegalArgumentException("Unknown simulation type requested '" + simulationType + "'.");
        }
    }
}
