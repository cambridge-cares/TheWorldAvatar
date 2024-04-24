package uk.ac.cam.cares.jps.base.slurm.job;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class PostProcessingTest {
    private Path tempDir;

    @BeforeEach
    void createTempDir() throws IOException {
        // This directory will be deleted at the end of each test.
        tempDir = Files.createTempDirectory("PostProcessingTest");
    }

    @AfterEach
    void deleteTempDir() throws IOException {
        if (null != tempDir) {
            FileUtils.forceDelete(tempDir.toFile());
        }
    }

    @Test
    void testupdateJobOutputStatus() throws IOException {
        // Create the status.txt file.
        Path tempStatusFile = tempDir.resolve("jobDir").resolve("status.txt");
        Files.createDirectories(tempStatusFile.getParent());
        Files.write(tempStatusFile, "JobOutput: processed".getBytes());

        // Not a valid job folder as not "status.txt" file.
        File falseDir = tempDir.toFile();
        assertFalse(PostProcessing.updateJobOutputStatus(falseDir));
        // "status.txt" file should be detected.
        File trueDir = tempStatusFile.toFile().getParentFile();
        assertTrue(PostProcessing.updateJobOutputStatus(trueDir));
    }

    @Test
    void testmodifyOutputStatus() throws IOException {
        // Create the status.txt file.
        Path tempStatusFile = tempDir.resolve("jobDir").resolve("status.txt");
        Files.createDirectories(tempStatusFile.getParent());
        Files.write(tempStatusFile, "JobOutput: processed".getBytes());

        String status = "Try!!";
        PostProcessing.modifyOutputStatus(tempStatusFile.toString(), status);
        Files.readAllLines(tempStatusFile).forEach(line -> assertEquals("JobOutput: " + status, line));
    }
}