package uk.ac.cam.cares.jps.agent.kinetics.simulation.test;

import java.awt.Dimension;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.junit.Test;

import uk.ac.cam.cares.jps.agent.kinetics.simulation.KineticsAgent;

import org.junit.Assert;
import uk.ac.cam.cares.jps.agent.utils.ZipUtility;

public class KineticsAgentProcessOutputTest {

    /**
     * Performs a test of the agent's job post-processing using a sample output.zip archive.
     *
     * Note that this test will only function if the kinetics agent scripts have been correctly installed (using the SRM
     * backends "for_release" script) and structure of the agents directory matches the following (and is set in the
     * "agent.scripts.location" property within the kinetics-agent.properties file).
     *
     * - agent directory - simulation_templates - venv - Scripts - agkin_pre - agkin_post
     */
    @Test
    public void testJobSetup() {
        // Select the sample output.zip
        JFileChooser chooser = new JFileChooser(new File(System.getProperty("user.home")));
        chooser.setDialogTitle("Select sample 'output.zip'...");
        chooser.setFileFilter(new FileNameExtensionFilter("Sample Archive", "zip"));
        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

        JFrame tempFrame = new JFrame();
        tempFrame.setLocation(0, 0);
        tempFrame.setSize(new Dimension(0, 0));
        tempFrame.setVisible(true);

        if (chooser.showOpenDialog(tempFrame) == JFileChooser.APPROVE_OPTION) {
            File zipFile = chooser.getSelectedFile();

            try {
                // Temporary job directory
                Path tempJobFolder = Paths.get(System.getProperty("user.home"), "temp-" + System.currentTimeMillis());
                Files.createDirectories(tempJobFolder);

                Files.copy(
                        Paths.get(zipFile.getAbsolutePath()),
                        Paths.get(tempJobFolder.toString(), "output.zip")
                );

                // Initialise a dummy agent
                KineticsAgent agent = new KineticsAgent();
                agent.initAgentProperty();

                try {
                    // Run the post-processing section of the agent
                    boolean success = agent.postProcessing(tempJobFolder);
                    Assert.assertTrue("Post-processing scripts reported an issue!", success);

                    // TODO - Consider using the Apache Commons IO library to help here
                    Files.walk(tempJobFolder)
                            .sorted(Comparator.reverseOrder())
                            .map(Path::toFile)
                            .forEach(File::delete);

                } catch (IOException ioException) {
                    ioException.printStackTrace(System.out);
                    Assert.fail("Could not delete temporary job folder!");
                }

            } catch (Exception exception) {
                exception.printStackTrace(System.out);
                Assert.fail("Exception when running postProcessing() method!");
            }

        } else {
            Assert.fail("Could not locate sample 'output.zip' archive!");
        }
    }
}
