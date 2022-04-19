package com.cmclinnovations.mods.modssimpleagent;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class MoDSBackendFactory {

    private static final Map<String, MoDSBackend> backendRegister = new ConcurrentHashMap<>();

    /**
     *
     */
    private static final Logger LOGGER = LogManager.getLogger(MoDSBackendFactory.class);

    private MoDSBackendFactory() {
    }

    public static MoDSBackend createMoDSBackend() throws IOException {

        Path simDir = Files.createTempDirectory("mods-sim-");
        String jobID = simDir.getFileName().toString();
        MoDSBackend moDSBackend = new MoDSBackend(jobID, simDir, TimeUnit.SECONDS.convert(5L, TimeUnit.MINUTES));
        backendRegister.put(jobID, moDSBackend);

        return moDSBackend;
    }

    static MoDSBackend getMoDSBackend(String jobID) {
        return backendRegister.get(jobID);
    }
}
