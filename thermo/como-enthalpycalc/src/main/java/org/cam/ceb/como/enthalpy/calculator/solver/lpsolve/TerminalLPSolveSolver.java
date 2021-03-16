/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.solver.lpsolve;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.exec.CommandLine;
import org.apache.commons.exec.DefaultExecutor;
import org.apache.commons.exec.ExecuteException;
import org.apache.commons.exec.ExecuteWatchdog;
import org.apache.commons.exec.Executor;
import org.apache.commons.exec.PumpStreamHandler;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.LpSolverException;
import org.cam.ceb.como.enthalpy.calculator.solver.NoFeasibleSolutionException;

/**
 *
 * @author pb556
 */
public class TerminalLPSolveSolver extends LPSolver {

    private Logger logger = Logger.getLogger(getClass());

    public TerminalLPSolveSolver() {
    }

    public TerminalLPSolveSolver(long timeout) {
        super(timeout);
    }

    public TerminalLPSolveSolver(boolean delInputFile, boolean delOutputFile) {
        super(delInputFile, delOutputFile);
    }

    public TerminalLPSolveSolver(long timeout, boolean delInputFile, boolean delOutputFile) {
        super(timeout, delInputFile, delOutputFile);
    }

    @Override
    public Map<String, Number> solve(File lpInputFile) throws LpSolverException {
        int exitValue = Integer.MIN_VALUE;
        File input = lpInputFile;
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("lp_solve", "W:\\Applications\\molhub\\chom-isodesmic\\src\\main\\resources\\gigadot\\chom\\isodesmic\\lp_solve\\win32\\lp_solve.exe"); //"lp_solve");
        map.put("input", input);
        CommandLine commandLine = CommandLine.parse("${lp_solve} ${input}", map);
        logger.trace("Command line to be executed --> " + commandLine);

        Executor executor = new DefaultExecutor();
        if (lpInputFile.getParentFile().canWrite()) {
            executor.setWorkingDirectory(lpInputFile.getParentFile());
        }
        executor.setExitValue(0);

        // create a watchdog if requested
        ExecuteWatchdog watchdog = new ExecuteWatchdog(timeout);
        ByteArrayOutputStream stdout = new ByteArrayOutputStream();
        PumpStreamHandler psh = new PumpStreamHandler(stdout);
        executor.setStreamHandler(psh);
        executor.setWatchdog(watchdog);
        try {
            exitValue = executor.execute(commandLine);
            if (!executor.isFailure(exitValue)) {
                return parseLpSolveOutput(stdout);
            }
            return new HashMap<String, Number>();
        } catch (ExecuteException ex) {
            throw new LpSolverException("Failed to execute the command (exit value = " + exitValue + ") : " + commandLine, ex);
        } catch (IOException ex) {
            throw new LpSolverException("Failed to execute the command due to I/O : " + commandLine, ex);
        } finally {
            if (delInput) {
                    FileUtils.deleteQuietly(input);
                    input.deleteOnExit();
                    //Runtime.getRuntime().exec("rm -f " + input);
            }
        }
    }

    private Map<String, Number> parseLpSolveOutput(ByteArrayOutputStream stdout) throws LpSolverException {
        Map<String, Number> solutions = new HashMap<String, Number>();
        boolean startParsing = false;
        List<String> readLines;
        try {
            readLines = IOUtils.readLines(new ByteArrayInputStream(stdout.toByteArray()), "UTF-8");
        } catch (IOException ex) {
            throw new LpSolverException("Failed to read the output : " + stdout.toString(), ex);
        }
        for (String string : readLines) {
            if (startParsing) {
                if (string.contains("infeasible")) {
                    throw new NoFeasibleSolutionException("This problem is infeasible.");
                }
                String[] split = string.split("\\s+");
                Number num;
                try {
                    num = Integer.parseInt(split[1]);
                } catch (NumberFormatException nex) {
                    try {
                        num = Double.parseDouble(split[1]);
                    } catch (NumberFormatException nex2) {
                        throw new LpSolverException("Unable to parse a number " + split[1]);
                    }
                }
                solutions.put(split[0], num);

            }
            startParsing = (startParsing) ? true : string.contains("Actual values of the variables:");
        }
        return solutions;
    }

    @Override
    public Map<String, Number> solve() throws LpSolverException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
