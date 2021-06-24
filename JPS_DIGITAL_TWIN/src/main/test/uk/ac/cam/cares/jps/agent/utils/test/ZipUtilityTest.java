package uk.ac.cam.cares.jps.agent.utils.test;

import org.apache.commons.io.FileUtils;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import uk.ac.cam.cares.jps.agent.utils.ZipUtility;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class ZipUtilityTest {

    @Test
    public void testNewZipUtility() {
        ZipUtility zipUtility = null;
        try {
            zipUtility = new ZipUtility();
        } finally {
            Assert.assertNotNull(zipUtility);
        }
    }

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void testZipUnzip() throws IOException {
        // Create a test file
        File file = folder.newFile("test.txt");
        // Write something in the file
        FileWriter writer = new FileWriter(file);
        writer.write("hello world");
        writer.close();

        // Create a test folder with a file inside
        File subFolder = folder.newFolder("subfolder");
        File file2 = new File(subFolder.getAbsolutePath()+"/test2.txt");
        file2.createNewFile();

        // Add test file and folder to list of files that should be zipped
        List<File> filesList = new ArrayList();
        filesList.add(file) ;
        filesList.add(subFolder);

        ZipUtility zipUtility = new ZipUtility();
        // Zip the files created above
        File zipFile = folder.newFile("test.zip");
        zipUtility.zip(filesList, zipFile.getAbsolutePath());
        // Unzip the files back in a new folder
        File zipFolder = folder.newFolder("zipfolder");
        zipUtility.unzip(zipFile.getAbsolutePath(), zipFolder.getAbsolutePath());

        // Get all files in the extracted folder
        String files[] = zipFolder.list();
        Assert.assertEquals(2, files.length);
        // Read from the unzipped test file
        String tmpFile = zipFolder.getAbsolutePath()+"/"+file.getName();
        Scanner reader = new Scanner(new File(tmpFile));
        Assert.assertEquals("hello world", reader.nextLine());
        reader.close();
    }

}
