/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.openbabel.wrapper;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.exec.CommandLine;
import org.apache.commons.exec.DefaultExecutor;
import org.apache.commons.exec.ExecuteWatchdog;
import org.apache.commons.exec.Executor;
import org.apache.commons.exec.PumpStreamHandler;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.SystemUtils;

import uk.ac.cam.ceb.como.openbabel.cml.OpenBabelException;
import uk.ac.cam.ceb.como.openbabel.util.OpenBabelUtil;

/**
 *
 * @author pb556
 * 
 */
public class OpenBabelWrapper {

    private static File tempDir = SystemUtils.getUserHome();
    private static boolean executableExists = false;

    static {
        executableExists = OpenBabelUtil.executableExists();
    }

    public static synchronized int execute(String inputType, String outputType, File input, File output, String arguments) throws OpenBabelException {
        if (!executableExists) {
            throw new OpenBabelException("babel command not found on your system. If you have just"
                    + " installed Open Babel, please restart your application or redeploy your web application"
                    + " or explicitly call method executableExists().");
        }
        int exitValue = Integer.MIN_VALUE;
        Map map = new HashMap();
        map.put("input", input);
        map.put("inputType", inputType);
        map.put("output", output);
        map.put("outputType", outputType);
        map.put("arguments", arguments);
        CommandLine commandLine = CommandLine.parse("babel -i${inputType} ${input} -o${outputType} ${output} ${arguments}", map);

        ExecuteWatchdog watchdog = new ExecuteWatchdog(60000);
        Executor executor = new DefaultExecutor();
        executor.setExitValue(0);
        ByteArrayOutputStream stdout = new ByteArrayOutputStream();
        PumpStreamHandler psh = new PumpStreamHandler(stdout);
        executor.setStreamHandler(psh);
        executor.setWatchdog(watchdog);
        try {
            exitValue = executor.execute(commandLine);
        } catch (Exception ex) {
            throw new OpenBabelException("Failed to execute command (exit value = " + exitValue + ") : " + commandLine.toString(), ex);
        }
        return exitValue;
    }
    
    public static synchronized List<String> execute(String inputType, String outputType, File input, String arguments) throws OpenBabelException {
        String outputFileName = UUID.randomUUID().toString();
        File tmp = getTempDir();
        File output = new File(tmp.getAbsolutePath() + "/" + outputFileName + "." + outputType);
        execute(inputType, outputType, input, output, arguments);
        List<String> content = new ArrayList<String>();
        try {
            content = FileUtils.readLines(output, "UTF-8");
        } catch (IOException ex) {
            Logger.getLogger(OpenBabelWrapper.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            FileUtils.deleteQuietly(tmp);
        }
        return content;
    }

    public static void setTempDir(File dir) {
        tempDir = dir;        
    }
    
    private static File getTempDir() {
        File f = new File(tempDir, "/.jbabel/converter/" + UUID.randomUUID());
        f.mkdirs();
        return f;
    }
}