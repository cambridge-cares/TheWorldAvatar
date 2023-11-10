package com.cmclinnovations.mods.modssimpleagent.simulations;

import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import com.cmclinnovations.mods.api.MoDSAPI;
import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Algorithm;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.utils.SimulationLoader;
import com.cmclinnovations.mods.modssimpleagent.utils.SimulationSaver;

class DKL extends Simulation {

    public DKL(Request request, BackendInputFile inputFile, MoDSBackend modsBackend, InputMetaData inputMetaData,
            SimulationSaver simulationSaver, SimulationLoader simulationLoader) {
        super(request, inputFile, modsBackend, inputMetaData, simulationSaver, simulationLoader);
    }

    @Override
    protected Algorithm getPrimaryAlgorithm() {
        return getAlgorithmOfType("GenSurrogateAlg");
    }

    @Override
    protected void generateFiles() throws FileGenerationException {
        generateInitialFileFromInputs();
        generateDataAlgFiles();
        super.generateFiles();
    }

    @Override
    public Request getResults() {

        String simDir = getModsBackend().getSimDir().toString();
        String algorithmName = MoDSBackend.DEFAULT_SURROGATE_ALGORITHM_NAME;

        if (!MoDSAPI.hasAlgorithmGeneratedOutputFiles(simDir, algorithmName)) {
            throw new ResponseStatusException(
                    HttpStatus.NO_CONTENT,
                    "The Deep Kernel Learning job with job id '" + getModsBackend().getJobID()
                            + "' has not finished yet, has not been run or has failed to run correctly.");
        }

        return super.getResults();
    }

}
