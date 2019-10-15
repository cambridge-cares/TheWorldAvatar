/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.openbabel.util;

import org.cam.ceb.como.openbabel.wrapper.OpenBabelWrapper;
import java.io.ByteArrayOutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.exec.CommandLine;
import org.apache.commons.exec.DefaultExecutor;
import org.apache.commons.exec.ExecuteWatchdog;
import org.apache.commons.exec.Executor;
import org.apache.commons.exec.PumpStreamHandler;

/**
 *
 * @author pb556
 */
public class OpenBabelUtil {
    
    private static String version = "";
    private static boolean executableExists = false;
    
    static {
        System.loadLibrary("openbabel_java");
        executableExists = executableExists();
    }

    public static synchronized boolean executableExists() {
        CommandLine commandLine = CommandLine.parse("babel -v");

        ExecuteWatchdog watchdog = new ExecuteWatchdog(10000);

        Executor executor = new DefaultExecutor();
        executor.setExitValue(0);
        executor.setWatchdog(watchdog);

        ByteArrayOutputStream stdout = new ByteArrayOutputStream();
        ByteArrayOutputStream stderr = new ByteArrayOutputStream();
        PumpStreamHandler psh = new PumpStreamHandler(stdout, stderr);
        executor.setStreamHandler(psh);
        try {
            executor.execute(commandLine);
            String s = stdout.toString();
            executableExists = s.contains("Open Babel");
            version = s.substring(s.indexOf("Open Babel"),
                    s.indexOf('\n', s.indexOf("Open Babel")));
            Logger.getLogger(OpenBabelWrapper.class.getName()).log(Level.INFO, version);
        } catch (Exception ex) {
            executableExists = false;
            Logger.getLogger(OpenBabelWrapper.class.getName()).log(Level.SEVERE,
                    "Open Babel is not installed!");
        }
        return executableExists;
    }
    
    public static String getVersion() {
        return version;
    }
}
