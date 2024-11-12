package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import com.cmclinnovations.mods.api.MoDSAPI;
import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Algorithm;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Variable;
import com.cmclinnovations.mods.modssimpleagent.utils.SimulationLoader;
import com.cmclinnovations.mods.modssimpleagent.utils.SimulationSaver;

class MOOonly extends Simulation {

    public MOOonly(Request request, BackendInputFile inputFile, MoDSBackend modsBackend, InputMetaData inputMetaData,
            SimulationSaver simulationSaver, SimulationLoader simulationLoader) {
        super(request, inputFile, modsBackend, inputMetaData, simulationSaver, simulationLoader);
    }

    @Override
    protected Algorithm getPrimaryAlgorithm() {
        return getAlgorithmOfType("MOO");
    }

    @Override
    protected void populateAlgorithmNodes(List<Variable> variables) {
        populateMOOAlgorithmNode(Simulation.DEFAULT_MOO_ALGORITHM_NAME, getPrimaryAlgorithm().name(), variables);
        super.populateAlgorithmNodes(variables);
    }

    @Override
    protected void generateFiles() throws FileGenerationException {
        generateInitialFileFromMetaData();
        super.generateFiles();
    }

    @Override
    public Request getResults() {
        String simDir = getModsBackend().getSimDir().toString();
        if (!MoDSAPI.hasAlgorithmGeneratedOutputFiles(simDir, DEFAULT_MOO_ALGORITHM_NAME)) {
            throw new ResponseStatusException(
                    HttpStatus.NO_CONTENT,
                    "The multi-objective optimisation job with job '" + getModsBackend().getJobID()
                            + "' has not finished yet, has not been run, or has failed to run correctly.");
        }
        return getMCDMResults();
    }
}
