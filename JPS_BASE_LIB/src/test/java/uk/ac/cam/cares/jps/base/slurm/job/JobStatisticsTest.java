package uk.ac.cam.cares.jps.base.slurm.job;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import java.io.*;
import java.net.URISyntaxException;
import static org.junit.jupiter.api.Assertions.*;

class JobStatisticsTest {

    public static void generatefile() throws IOException {
        String thisdir = System.getProperty("user.dir"); //Returns JPS_BASE_LIB
        String fileloc = thisdir + "\\src\\test\\resources\\JobStatisticsTestInputstatus.txt"; //Navigate to the resources folder define the name of the test file. Not strictly necessary given that the status file is temporary (i.e. it can go anywhere), but historically this is where we put the test file
        File tempInputStatus = new File(fileloc); //Create JobstatisticsTestInputstatus.txt. This file will be deleted at the end of the unit test.
        FileWriter writer = new FileWriter(tempInputStatus);

        //Fill JobStatisticsTestInputstatus.txt with text. We have one JobStatus: completed, two JobStatus: completing, etc.
        for (int i = 0; i < 1; i++) {
            writer.write("JobStatus: completed");
        }

        for (int i = 0; i < 2; i++) {
            writer.write(System.getProperty( "line.separator" )); //This puts the next JobStatus on a new line
            writer.write("JobStatus: completing");
        }

        for (int i = 0; i < 3; i++) {
            writer.write(System.getProperty( "line.separator" ));
            writer.write("JobStatus: failed");
        }

        for (int i = 0; i < 4; i++) {
            writer.write(System.getProperty( "line.separator" ));
            writer.write("JobStatus: pending");
        }

        for (int i = 0; i < 5; i++) {
            writer.write(System.getProperty( "line.separator" ));
            writer.write("JobStatus: preempted");
        }

        for (int i = 0; i < 6; i++) {
            writer.write(System.getProperty( "line.separator" ));
            writer.write("JobStatus: running");
        }

        for (int i = 0; i < 7; i++) {
            writer.write(System.getProperty( "line.separator" ));
            writer.write("JobStatus: suspended");
        }

        for (int i = 0; i < 8; i++) {
            writer.write(System.getProperty( "line.separator" ));
            writer.write("JobStatus: stopped");
        }

        for (int i = 0; i < 9; i++) {
            writer.write(System.getProperty( "line.separator" ));
            writer.write("JobStatus: error termination");
        }

        for (int i = 0; i < 10; i++) {
            writer.write(System.getProperty( "line.separator" ));
            writer.write("JobStatus: not started");
        }

        writer.close();
    }

    @Test
    @DisplayName("Testing calculateStatistics")
    void calculateStatistics() throws IOException, URISyntaxException {
        generatefile(); //Creates JobStatisticsTestInputstatus.txt in the \\src\\test\\resources folder

        String thisdir = System.getProperty("user.dir"); //Returns JPS_BASE_LIB
        String fileloc = thisdir + "\\src\\test\\resources\\JobStatisticsTestInputstatus.txt"; //Navigate to the resources folder, which is where generatefile puts the test file

        //We define two Files: one for deleting JobStatisticsTestInputstatus.txt at the end of the test, and the other to call JobStatistics (since JobStatistics takes the "job workspace folder" as input)
        File tempInputStatus = new File(fileloc); //Call JobstatisticsTestInputstatus.txt, which was created by generatefile(). This file will be deleted at the end of the unit test.
        File input = new File(thisdir); //To be used for calling JobStatistics

        JobStatistics result = new JobStatistics(input); //Create new object
        result.calculateStatistics(fileloc); //And feed JobStatisticsTestInputstatus.txt as the input. The file has 1 job completed, 2 jobs completing, etc, hence all of the assertEquals must be true for the test to be successful

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

        tempInputStatus.delete(); //Delete the test file we created earlier, returning the folder to its original condition
    }

    @Test
    @DisplayName("Testing JobStatistics")
    void JobStatistics() throws IOException {
        generatefile(); //Creates JobStatisticsTestInputstatus.txt in the \\src\\test\\resources folder

        String thisdir = System.getProperty("user.dir"); //Returns the working directory, which is JPS_BASE_LIB
        String newdir = thisdir + "\\src\\test"; //Now go to JPS_BASE_LIB\src\test, because in JPS_BASE_LIB\src\test\resources is where the test input file is, and the JobStatistics method in JobStatistics.java searches two folders deep
        String fileloc = thisdir + "\\src\\test\\resources\\JobStatisticsTestInputstatus.txt"; //Navigate to the resources folder, which is where generatefile puts the test file
                                                                                                //Notably the test file must end in status.txt or it will not be detected by calculateStatistics (line 71 in Jobstatistics.java, endsWith(Status.STATUS_FILE.getName()).)

        //We make two Files: one to delete JobStatisticsTestInputstatus.txt at the end of the test, and the other to call JobStatistics (since JobStatistics takes the "job workspace folder" as input)
        File tempInputStatus = new File(fileloc); //Create JobstatisticsTestInputstatus.txt. This file will be deleted at the end of the unit test.
        File input = new File(newdir); //Use the new path as input

        JobStatistics result = new JobStatistics(input);

        assertEquals(55, result.getJobsSubmitted()); //There are 55 jobs total (= 1 + 2 + 3 + 4 + ... + 10). 1 completed, 2 completing, 3 failed, 4 pending, etc. getJobsSubmitted should therefore return 55.

        tempInputStatus.delete();
    }
}