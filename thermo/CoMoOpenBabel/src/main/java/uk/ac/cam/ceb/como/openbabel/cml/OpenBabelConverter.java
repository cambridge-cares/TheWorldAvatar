/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.openbabel.cml;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import org.apache.commons.exec.CommandLine;
import org.apache.commons.exec.DefaultExecutor;
import org.apache.commons.exec.ExecuteWatchdog;
import org.apache.commons.exec.Executor;
import org.apache.commons.exec.PumpStreamHandler;
import org.apache.commons.io.FileUtils;

/**
 *
 * @author pb556
 */
public class OpenBabelConverter {
    
    public static String convert(CommandLine commandLine, File input, File output, boolean delInput, boolean delOutput) throws OpenBabelException {
        int exitValue = Integer.MIN_VALUE;
        Executor executor = new DefaultExecutor();
        executor.setExitValue(0);

        // create a watchdog if requested
        long timeout = 60000;
        ExecuteWatchdog watchdog = new ExecuteWatchdog(timeout);
        ByteArrayOutputStream stdout = new ByteArrayOutputStream();
        PumpStreamHandler psh = new PumpStreamHandler(stdout);
        executor.setStreamHandler(psh);
        executor.setWatchdog(watchdog);
        try {
            exitValue = executor.execute(commandLine);
        } catch (Exception ex) {
            throw new OpenBabelException("Failed to execute command (exit value = " + exitValue + ") : " + commandLine.toString(), ex);
        }
        try {
            return FileUtils.readFileToString(output, "UTF-8");
        } catch (IOException ex) {
            throw new OpenBabelException("Unable to read output file at " + output.getAbsolutePath(), ex);
        } finally {
            if (delInput) {
                FileUtils.deleteQuietly(input);
            }
            if (delOutput) {
                FileUtils.deleteQuietly(output);
            }
        }
    }
}
