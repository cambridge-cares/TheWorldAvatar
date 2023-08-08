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
    private final long killTimeout = 10;

    private Process process;

    public MoDSBackend(String jobID, Path simDir, long timeout) throws IOException {
        this.jobID = jobID;

        this.simDir = simDir;

        allDir = createSubDir("All");
        initialDir = createSubDir("Initial");
        workingDir = createSubDir("Working_dir");

        this.timeout = timeout;
    }

    public String getJobID() {
        return jobID;
    }

    public Path createSubDir(String subDirName) throws IOException {
        Path subDirPath = simDir.resolve(subDirName);
        if (!Files.exists(subDirPath)) {
            Files.createDirectory(subDirPath);
        }
        return subDirPath;
    }

    public void run() throws IOException {
        LOGGER.info("simDir = '{}'", simDir);

        if (isAlive()) {
            throw new ResponseStatusException(
                    HttpStatus.CONFLICT,
                    "Process with job ID '" + jobID + "' already running.");
        }

        process = new ProcessBuilder(MODS_EXE_FILENAME, "--gui")
                .directory(simDir.toFile())
                .inheritIO()
                .start();

        Thread timoutEnforcer = new Thread(() -> {
            try {
                if (!process.waitFor(timeout, TimeUnit.SECONDS)) { // let the process run for 'timeout' seconds
                    LOGGER.error("MoDS process has timed out after '{}' seconds so will be killed.", timeout);
                    process.destroy(); // tell the process to stop
                    if (!process.waitFor(killTimeout, TimeUnit.SECONDS)) { // give it a chance to stop
                        LOGGER.error("MoDS process could not be killed within '{}' seconds so will be killed forcibly.",
                                killTimeout);
                        process.destroyForcibly(); // tell the OS to kill the process
                        process.waitFor(); // the process is now dead
                    }
                } else {
                    long exitValue = Integer.toUnsignedLong(process.exitValue());
                    if (0L == exitValue) {
                        LOGGER.info("MoDS process finished successfully.");
                    } else {
                        LOGGER.error("MoDS process exited with error code '{}'.", exitValue);
                    }
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt(); // set the flag back to true
            }
        }, "timout-enforcer-" + jobID);
        timoutEnforcer.start();
    }

    public boolean isAlive() {
        return null != process && process.isAlive();
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
