package com.cmclinnovations.mods.modssimpleagent;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class MoDSBackendFactory {

    private static final Logger LOGGER = LogManager.getLogger(MoDSBackendFactory.class);

    private static final Path TEMP_DIR = Paths.get(System.getProperty("java.io.tmpdir"));
    private static final String SIM_DIR_PREFIX = "mods-sim-";

    private MoDSBackendFactory() {
    }

    public static MoDSBackend createMoDSBackend() throws IOException {

        Path simDir = Files.createTempDirectory(TEMP_DIR, SIM_DIR_PREFIX);
        String jobID = simDir.getFileName().toString();
        return new MoDSBackend(jobID, simDir, TimeUnit.SECONDS.convert(5L, TimeUnit.MINUTES));
    }

    public static MoDSBackend retrieveMoDSBackend(String jobID) throws IOException {
        Path simDir = TEMP_DIR.resolve(jobID);
        return new MoDSBackend(jobID, simDir, -1);
    }
}
