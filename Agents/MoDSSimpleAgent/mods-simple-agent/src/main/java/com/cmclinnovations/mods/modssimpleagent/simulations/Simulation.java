package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.IOException;

import javax.xml.bind.JAXBException;

import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackendFactory;
import com.cmclinnovations.mods.modssimpleagent.TemplateLoader;

public class Simulation {

    private final BackendInputFile inputFile;
    private final MoDSBackend modsBackend;

    public static Simulation createSimulation(String simulationType) throws JAXBException, IOException {

        BackendInputFile inputFile = TemplateLoader.load(simulationType);
        MoDSBackend modsBackend = MoDSBackendFactory.createMoDSBackend();
        switch (simulationType) {
            case "MOO":
                return new MOO(inputFile, modsBackend);
            default:
                throw new IllegalArgumentException("Unknown simulation type requested '" + simulationType + "'.");
        }
    }

    public Simulation(BackendInputFile inputFile, MoDSBackend modsBackend) {
        this.inputFile = inputFile;
        this.modsBackend = modsBackend;

    }

    public void run() throws IOException, JAXBException {
        inputFile.marshal(modsBackend.getWorkingDir().resolve(BackendInputFile.FILENAME));
        modsBackend.run();
    }

}
