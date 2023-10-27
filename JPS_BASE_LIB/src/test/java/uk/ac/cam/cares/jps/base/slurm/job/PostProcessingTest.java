package uk.ac.cam.cares.jps.base.slurm.job;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Scanner;

import org.junit.jupiter.api.Test;


class PostProcessingTest {

    @Test
    void testupdateJobOutputStatus() throws IOException {
        String thisdir = System.getProperty("user.dir"); //Returns JPS_BASE_LIB
        String newdir = thisdir + "\\src\\test\\resources"; //Amend directory to JPS_BASE_LIB\src\test\resources, which is where we put the test file.

        File tempstatusfile = new File(newdir + "\\status.txt"); //Create the status.txt file in JPS_BASE_LIB\src\test\resources. This file will be deleted at the end
        FileWriter writer = new FileWriter(tempstatusfile);
        writer.write("JobOutput: processed"); //Then write "JobOutput: processed into status.txt
        writer.close();

        File falseinput = new File(thisdir); //If we feed only JPS_BASE_LIB, getStatusFile should NOT work, and therefore updateJobOutputStatus should not run. In this case the expected output is false.
        File trueinput = new File(newdir); //If we feed JPS_BASE_LIB\src\test\resources, status.txt should be detected and updateJobOutputStatus should run.
        boolean testres = PostProcessing.updateJobOutputStatus(trueinput);
        assertEquals(true, testres); //The code returns "true" if updateJobOutputStatus successfully runs, so this is the only thing we can test
        boolean testres2 = PostProcessing.updateJobOutputStatus(falseinput);
        assertEquals(false,testres2);
        tempstatusfile.delete(); //Delete the created status.txt file. This returns the folder to its original state
    }

    @Test
    void testmodifyOutputStatus() throws IOException {
        String thisdir = System.getProperty("user.dir"); //Returns JPS_BASE_LIB
        String newdir = thisdir + "\\src\\test\\resources"; //Amend directory to JPS_BASE_LIB\src\test\resources, which is where we put the test file.

        File tempstatusfile = new File(newdir + "\\status.txt"); //Create the status.txt file in JPS_BASE_LIB\src\test\resources. This file will be deleted at the end
        FileWriter writer = new FileWriter(tempstatusfile);
        writer.write("JobOutput: processed"); //Then write "JobOutput: processed into status.txt
        writer.close();

        //Unlike updateJobOutputStatus, openSourceFile in modifyOutputStatus requires the name of the file. Therefore, we need to redefine the string newdir again.
        newdir = newdir + "\\status.txt";

        //Define this file as the input for the Scanner method called later
        File input = new File(newdir);

        PostProcessing.modifyOutputStatus(newdir,"Try!!");
        Scanner output = new Scanner(input); //Used to read the input file
        String outputstring = output.nextLine(); //Get the text in file. modifyOutputStatus actually sets *all* the JobOutputs to "Try!!", so it doesn't really matter where the pointer currently is
        assertEquals(" Try!!", outputstring.substring(outputstring.lastIndexOf(":") + 1)); //We defined the new status as "Hello world" earlier, so we make this assertion. There is an extra space in the expected result because there's a space after : in status.txt

        output.close(); //Need to close Scanner or there's a compilation error
        tempstatusfile.delete(); //Delete the created status.txt file. This returns the folder to its original state
    }
}