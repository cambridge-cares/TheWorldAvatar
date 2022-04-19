package com.cmclinnovations.mods.modssimpleagent;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

public final class MoDSBackend {

    private static final Logger LOGGER = LogManager.getLogger(MoDSBackend.class);
    private static final String MODS_EXE_FILENAME = "MoDS_mpi";

    private final String jobID;
    private final Path simDir;
    private final Path allDir;
    private final Path initialDir;
    private final Path workingDir;

    private final long timeout;

    private Process process;

    public MoDSBackend(String jobID, Path simDir, long timeout) throws IOException {
        this.jobID = jobID;

        this.simDir = simDir;

        allDir = simDir.resolve("All");
        Files.createDirectory(allDir);
        initialDir = simDir.resolve("Initial");
        Files.createDirectory(initialDir);
        workingDir = simDir.resolve("Working_dir");
        Files.createDirectory(workingDir);

        this.timeout = timeout;
    }

    public void run() throws IOException {
        LOGGER.info("simDir = '{}'", simDir);

        if (null != process && process.isAlive()) {
            throw new ResponseStatusException(
                    HttpStatus.CONFLICT,
                    "Process with job ID '" + jobID + "' already running.");
        }

        ProcessBuilder pb = new ProcessBuilder(MODS_EXE_FILENAME, "--gui");
        pb.directory(simDir.toFile());
        pb.inheritIO();

        process = pb.start();

        Runnable timoutEnforcer = () -> {
            try {
                process.waitFor(timeout, TimeUnit.SECONDS); // let the process run for 5 seconds
                process.destroy(); // tell the process to stop
                process.waitFor(10, TimeUnit.SECONDS); // give it a chance to stop
                process.destroyForcibly(); // tell the OS to kill the process
                process.waitFor(); // the process is now dead
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt(); // set the flag back to true
            }
        };
        timoutEnforcer.run();
    }

    public void checkState() {
        if (null == process) {
            throw new ResponseStatusException(
                    HttpStatus.CONFLICT,
                    "Process with job ID '" + jobID + "' has not been created yet.");
        }
    }

    public Path getSimDir() {
        return simDir;
    }

    public Path getAllDir() {
        return allDir;
    }

    public Path getInitialDir() {
        return initialDir;
    }

    public Path getWorkingDir() {
        return workingDir;
    }

}
