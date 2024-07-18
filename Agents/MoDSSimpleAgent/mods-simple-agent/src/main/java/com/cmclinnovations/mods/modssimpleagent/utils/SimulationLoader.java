package com.cmclinnovations.mods.modssimpleagent.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.simulations.Simulation;

public class SimulationLoader {

    private static final Logger LOGGER = LogManager.getLogger(SimulationLoader.class);
    private final MoDSBackend modsBackend;

    public SimulationLoader(MoDSBackend modsBackend) {
        this.modsBackend = modsBackend;
    }

    public void loadSurrogate(String surrogateToLoad) {
        Path surrogateDirectory = modsBackend.getSurrogateDirectory();
        Path loadDirectory = Simulation.getSurrogateSaveDirectoryPath().resolve(surrogateToLoad)
                .resolve(MoDSBackend.DEFAULT_SURROGATE_ALGORITHM_NAME);
        try {
            if (!Files.exists(loadDirectory)) {
                throw new IOException(
                        "File '" + loadDirectory.toAbsolutePath() + "' could not be found to load.");
            }

            FileUtils.copyDirectory(loadDirectory, surrogateDirectory);

            LOGGER.info("File '{}' loaded to '{}'.", loadDirectory.toAbsolutePath(),
                    surrogateDirectory.toAbsolutePath());

        } catch (IOException ex) {
            LOGGER.error("Failed to load '{}' to '{}'.", loadDirectory.toAbsolutePath(),
                    surrogateDirectory.toAbsolutePath(), ex);
            throw new ResponseStatusException(HttpStatus.NOT_FOUND,
                    "Job '" + modsBackend.getJobID() + "' failed to load.", ex);
        }
    }
}
