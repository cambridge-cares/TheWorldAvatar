package uk.ac.cam.cares.jps.base.slurm.job.test;

import static org.junit.jupiter.api.Assertions.*;
import uk.ac.cam.cares.jps.base.slurm.job.*;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Scanner;


class PostProcessingTest {

    @Test
    void testupdateJobOutputStatus() throws IOException {
        String thisdir = System.getProperty("user.dir"); //Returns JPS_BASE_LIB
        String newdir = thisdir + "\\src\\test\\resources"; //Now amend directory to JPS_BASE_LIB\src\test\resources, which is where we put the test file. Notably the test only works if the test file is named "status.txt"
        File falseinput = new File(thisdir); //If we feed only JPS_BASE_LIB, getStatusFile should NOT work, and therefore updateJobOutputStatus should not run. In this case the expected output is false.
        File trueinput = new File(newdir); //This is if we feed JPS_BASE_LIB\src\test\resources, in which case status.txt should be detected and updateJobOutputStatus should run.
        boolean testres = PostProcessing.updateJobOutputStatus(trueinput);
        assertEquals(true, testres); //The code returns "true" if updateJobOutputStatus successfully runs, so this is the only thing we can test
        boolean testres2 = PostProcessing.updateJobOutputStatus(falseinput);
        assertEquals(false,testres2);
    }

    @Test
    void testmodifyOutputStatus() throws IOException {
        String thisdir = System.getProperty("user.dir"); //Returns JPS_BASE_LIB
        String newdir = thisdir + "\\src\\test\\resources\\status.txt"; //Amend directory to JPS_BASE_LIB\src\test\resources, which is where we put the test file. Unlike the above, openSourceFile in modifyOutputStatus requires the name of the file
        File input = new File(newdir);

        PostProcessing.modifyOutputStatus(newdir,"Try!!");
        Scanner output = new Scanner(input); //Used to read the input file
        String outputstring = output.nextLine(); //Get the text in file. modifyOutputStatus actually sets *all* the JobOutputs to "Try!!", so it doesn't really matter where the pointer currently is
        assertEquals(" Try!!", outputstring.substring(outputstring.lastIndexOf(":") + 1)); //We defined the new status as "Hello world" earlier, so we make this assertion. There is an extra space in the expected result because there's a space after : in status.txt

        PostProcessing.modifyOutputStatus(newdir,"Hello? World"); //We modify OutputStatus again, because if we don't, it could be that OutputStatus isn't changing but the test passes anyway because a previous test had already modified status.txt to the correct result. The code is the same as previous.
        Scanner output2 = new Scanner(input);
        outputstring = output2.nextLine();
        assertEquals(" Hello? World", outputstring.substring(outputstring.lastIndexOf(":") + 1));
    }
}