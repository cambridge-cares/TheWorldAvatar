package uk.ac.cam.cares.jps.base.slurm.job.test;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.slurm.job.JobStatistics;

import java.io.*;
import java.net.URISyntaxException;
import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.*;

class JobStatisticsTest {

    @Test
    @DisplayName("Testing calculateStatistics")
    void calculateStatistics() throws IOException, URISyntaxException {
        String path = Paths.get(this.getClass().getResource("/JobStatisticsTestInputstatus.txt").toURI()).toFile().getPath(); //Returns the location of JobStatisticsTestInputstatus.txt, which is in JPS_BASE_LIB\src\test\resources\JobStatisticsTestInputstatus.txt
        File input = new File(System.getProperty("user.dir")); //Returns the working directory, which is JPS_BASE_LIB

        JobStatistics result = new JobStatistics(input); //Create new object
        result.calculateStatistics(path); //And feed JobStatisticsTestInputstatus.txt as the input. The file has 1 job completed, 2 jobs completing, etc, hence all of the assertEquals must be true for the test to be successful

        assertEquals(1,result.getJobsCompleted());
        assertEquals(2,result.getJobsCompleting());
        assertEquals(3,result.getJobsFailed());
        assertEquals(4,result.getJobsPending());
        assertEquals(5,result.getJobsPreempted());
        assertEquals(6,result.getJobsRunning());
        assertEquals(7,result.getJobsSuspended());
        assertEquals(8,result.getJobsStopped());
        assertEquals(9,result.getJobsErrorTerminated());
        assertEquals(10,result.getJobsNotStarted());
    }

    @Test
    void JobStatistics() throws IOException {
        String thisdir = System.getProperty("user.dir"); //Returns the working directory, which is JPS_BASE_LIB
        String newdir = thisdir + "\\src\\test"; //Now go to JPS_BASE_LIB\src\test, because in JPS_BASE_LIB\src\test\resources is where the test input file is, and the original code searches two folders deep
        File input = new File(newdir); //Use the new path as input
        JobStatistics result = new JobStatistics(input); //Create new object. JobStatistics

        assertEquals(55, result.getJobsSubmitted()); //The test file, Teststatus.txt, is the same as JobStatisticsTestInput.txt above. The "special" part of it is that it must end in status.txt, or it will not be detected by calculateStatistics (line 71 in Jobstatistics.java, endsWith(Status.STATUS_FILE.getName()) ).
                                                    //There are 55 jobs total (= 1 + 2 + 3 + 4 + ... + 10). 1 completed, 2 completing, 3 failed, 4 pending, etc. getJobsSubmitted should therefore return 55.
    }
}