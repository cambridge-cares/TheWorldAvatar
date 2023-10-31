package uk.ac.cam.cares.jps.base.slurm.job;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class JobStatisticsTest {
    @TempDir()
    static Path tempFolder;
    private static Path fileloc;

    // Create JobstatisticsTestInputstatus.txt. This file will be deleted at the end
    // of the unit test.
    @BeforeAll
    static void generatefile() throws IOException {
        Files.createDirectories(tempFolder);
        fileloc = tempFolder.resolve("jobFolder").resolve("JobStatisticsTestInputstatus.txt");
        File tempInputStatus = fileloc.toFile();
        tempInputStatus.getParentFile().mkdirs();
        try (FileWriter writer = new FileWriter(tempInputStatus)) {

            // Fill JobStatisticsTestInputstatus.txt with text. We have one JobStatus:
            // completed, two JobStatus: completing, etc.
            for (int i = 0; i < 1; i++) {
                writer.write("JobStatus: completed\n");
            }

            for (int i = 0; i < 2; i++) {
                writer.write("JobStatus: completing\n");
            }

            for (int i = 0; i < 3; i++) {
                writer.write("JobStatus: failed\n");
            }

            for (int i = 0; i < 4; i++) {
                writer.write("JobStatus: pending\n");
            }

            for (int i = 0; i < 5; i++) {
                writer.write("JobStatus: preempted\n");
            }

            for (int i = 0; i < 6; i++) {
                writer.write("JobStatus: running\n");
            }

            for (int i = 0; i < 7; i++) {
                writer.write("JobStatus: suspended\n");
            }

            for (int i = 0; i < 8; i++) {
                writer.write("JobStatus: stopped\n");
            }

            for (int i = 0; i < 9; i++) {
                writer.write("JobStatus: error termination\n");
            }

            for (int i = 0; i < 10; i++) {
                writer.write("JobStatus: not started\n");
            }
        }
    }

    @Test
    @DisplayName("Testing calculateStatistics")
    void calculateStatistics() throws IOException, URISyntaxException {
        // Call JobstatisticsTestInputstatus.txt, which was created by generatefile().
        // This file will be deleted at the end of the unit test.
        File tempInputStatus = fileloc.toFile();
        File input = tempInputStatus.getParentFile();

        // Create new object And feed JobStatisticsTestInputstatus.txt as the input. The
        // file has 1 job completed, 2 jobs completing, etc, hence all of the
        // assertEquals must be true for the test to be successful
        JobStatistics result = new JobStatistics(input);
        result.calculateStatistics(fileloc.toString());

        assertEquals(1, result.getJobsCompleted());
        assertEquals(2, result.getJobsCompleting());
        assertEquals(3, result.getJobsFailed());
        assertEquals(4, result.getJobsPending());
        assertEquals(5, result.getJobsPreempted());
        assertEquals(6, result.getJobsRunning());
        assertEquals(7, result.getJobsSuspended());
        assertEquals(8, result.getJobsStopped());
        assertEquals(9, result.getJobsErrorTerminated());
        assertEquals(10, result.getJobsNotStarted());
    }

    @Test
    @DisplayName("Testing JobStatistics")
    void JobStatistics() throws IOException {

        File tempInputStatus = fileloc.toFile();
        File input = tempInputStatus.getParentFile().getParentFile();

        JobStatistics result = new JobStatistics(input);
        // There are 55 jobs total (= 1 + 2 + 3 + 4 + ... + 10). 1 completed, 2
        // completing, 3 failed, 4 pending, etc. getJobsSubmitted should therefore
        // return 55.
        assertEquals(55, result.getJobsSubmitted());
    }
}