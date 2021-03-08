/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.extra.io;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Map;
import org.apache.commons.exec.CommandLine;
import org.apache.commons.exec.DefaultExecutor;
import org.apache.commons.exec.ExecuteException;
import org.apache.commons.exec.ExecuteWatchdog;
import org.apache.commons.exec.PumpStreamHandler;

/**
 *
 * @author pb556
 */
public final class Executor {

    public static int execute(CommandLine commandLine) throws ExecuteException, IOException {
        return execute(commandLine, 0l);
    }

    public static int execute(String cmdLine) throws ExecuteException, IOException {
        return execute(cmdLine, 0l);
    }

    public static int execute(String cmdLine, long timeout) throws ExecuteException, IOException {
        CommandLine commandLine = CommandLine.parse(cmdLine);
        return execute(commandLine, timeout);
    }

    public static int execute(String cmdLine, Map map) throws ExecuteException, IOException {
        return execute(cmdLine, map, 0l);
    }

    public static int execute(String cmdLine, Map map, long timeout) throws ExecuteException, IOException {
        CommandLine commandLine = CommandLine.parse(cmdLine, map);
        return execute(commandLine, timeout);
    }

    /**
     * <code>
     *  File input = new File(tempDir, "input.cml");
     *  File output = new File(tempDir, "output." + obformat.getObformat());
     *  Map map = new HashMap();
     *  map.put("input", input);
     *  map.put("obformat", obformat.getObformat());
     *  CommandLine commandLine = CommandLine.parse("babel -icml ${input} -o${obformat} ${output}", map);
     * </code>
     * @param commandLine
     * @param timeout
     * @return
     * @throws ExecuteException
     * @throws IOException
     */
    public static int execute(CommandLine commandLine, long timeout) throws ExecuteException, IOException {
        org.apache.commons.exec.Executor executor = new DefaultExecutor();
        executor.setExitValue(0);

        // create a watchdog if requested
        if (timeout > 0) {
            ExecuteWatchdog watchdog = new ExecuteWatchdog(timeout);
            executor.setWatchdog(watchdog);
        }

        ByteArrayOutputStream stdout = new ByteArrayOutputStream();
        ByteArrayOutputStream stderr = new ByteArrayOutputStream();
        PumpStreamHandler psh = new PumpStreamHandler(stdout, stderr);
        executor.setStreamHandler(psh);
        return executor.execute(commandLine);
    }
}
