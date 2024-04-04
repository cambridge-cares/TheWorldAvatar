package com.cmclinnovations.mods.modssimpleagent.utils;

import java.nio.file.Path;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.simulations.Simulation;

public class SimulationSaver {

    private final MoDSBackend modsBackend;
    private final InputMetaData inputMetaData;
    private static final Logger LOGGER = LogManager.getLogger(SimulationSaver.class);

    public SimulationSaver(MoDSBackend modsBackend, InputMetaData inputMetaData) {
        this.modsBackend = modsBackend;
        this.inputMetaData = inputMetaData;
    }

    public void saveSurrogate() {

        Path saveDirectory = Simulation.getSurrogateSaveDirectoryPath().resolve(modsBackend.getJobID())
                .resolve(MoDSBackend.DEFAULT_SURROGATE_ALGORITHM_NAME);
        Path surrogateDirectory = modsBackend.getSimDir().resolve(MoDSBackend.DEFAULT_SURROGATE_ALGORITHM_NAME);

        try {
            FileUtils.copyDirectory(surrogateDirectory, saveDirectory);
            inputMetaData.writeToCSV(saveDirectory.resolve(InputMetaData.DEFAULT_INPUT_INFO_FILE_NAME));
        } catch (FileGenerationException ex) {
            throw new ResponseStatusException(HttpStatus.NO_CONTENT,
                    "Job '" + modsBackend.getJobID() + "' failed to save.", ex);
        }

        LOGGER.info("Surrogate from job '{}' saved at '{}'.", modsBackend.getJobID(), saveDirectory.toAbsolutePath());

    }
}
