/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.apache.commons.exec.CommandLine;
import org.apache.commons.exec.DefaultExecutor;
import org.apache.commons.exec.ExecuteException;
import org.apache.commons.exec.ExecuteWatchdog;
import org.apache.commons.exec.Executor;
import org.apache.commons.exec.PumpStreamHandler;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.SystemUtils;
import org.apache.log4j.Logger;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LpSolverException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.NoFeasibleSolutionException;

/**
 *
 * @author pb556
 * 
 */
public class TerminalGLPKSolver extends LPSolver {

    private Logger logger = Logger.getLogger(getClass());

    public TerminalGLPKSolver() {
    	
    }

    public TerminalGLPKSolver(long timeout) {
        super(timeout);
    }

    public TerminalGLPKSolver(boolean delInputFile, boolean delOutputFile) {
        super(delInputFile, delOutputFile);
    }

    public TerminalGLPKSolver(long timeout, boolean delInputFile, boolean delOutputFile) {
        super(timeout, delInputFile, delOutputFile);
    }

    @Override
    public LinkedHashMap<String, Number> solve(File lpInputFile) throws LpSolverException {
    	
    	
    	
        int exitValue = Integer.MIN_VALUE;
        File tmp = getTempFile();
        
        Map<String, Object> map = new HashMap<String, Object>();
        //map.put("glpsol", "glpsol");
        //map.put("glpsol", "glpsol");
        //new File(getClass().getResource("").getPath());
        
        /**
         * 
         * @author nk510 (caresssd@hermes.cam.ac.uk)
         * Settings to run the code (LP solver) on local Windows (PC) machine. The GLPK solver should be copied (installed) in Java project ComoEnthalpyEstimationPaper.
         *  
         */
        map.put("glpsol", System.getProperty("user.dir") + "/glpk/w32/glpsol"); 

        /**
         * 
         * @author nk510 (caresssd@hermes.cam.ac.uk)
         * Settings to run the code (LP solver) on HPC. The GLPK solver should be installed (copied) on user's profile on Unix (Linux) HPC machine. The documentation of how use the GLPK is given on https://www.gnu.org/software/glpk/
         *  
         */
        /**
         * Added 'ebragent' beause comoenthalpyestimationpaper.jar file is stored in folder ebragent on users home directory.
         */
//        System.out.println("System.getProperty(user.dir): " + System.getProperty("user.dir")); - includes 'ebragent' folder: /home/nkrd01/ebragent
        System.out.println("SystemUtils.getUserHome(): "+ SystemUtils.getUserHome());
        
        //System.getProperty("user.dir")  -in previous verion of the code this was used. Now we use SystemUtils.getUserHome()
//        map.put("glpsol",  SystemUtils.getUserHome() + "/glpk-4.65/examples/glpsol");
        
        //"C:\Program Files\glpk-4.53\w32"
        //map.put("glpsol", "C:\\Program Files\\glpk-4.53\\w32\\glpsol");
        
        map.put("input_par", "--freemps");
        map.put("input", lpInputFile.getAbsolutePath());
        
        System.out.println("lpInputFile.getAbsolutePath(): " + lpInputFile.getAbsolutePath());
        
        map.put("output_par", "-o");
        map.put("output", tmp.getAbsolutePath());
        
        System.out.println("tmp.getAbsolutePath():" +tmp.getAbsolutePath());

        CommandLine commandLine = CommandLine.parse("${glpsol} ${input_par} ${input} ${output_par} ${output}", map);
        
//      System.out.println("Command line to be executed --> " + commandLine);
        
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
            	
                return parseLpSolveOutput(stdout, tmp);
                
            }
            
            return new LinkedHashMap<String, Number>();
        } catch (ExecuteException ex) {
            throw new LpSolverException("Failed to execute the command (exit value = " + exitValue + ") : " + commandLine, ex);
        } catch (IOException ex) {
            throw new LpSolverException("Failed to execute the command due to I/O : " + commandLine, ex);
        } finally {
            if (delInput) {
                FileUtils.deleteQuietly(lpInputFile);
                lpInputFile.deleteOnExit();
//                try {
//                    Runtime.getRuntime().exec("rm -f " + lpInputFile);
//                } catch (IOException ex) {
//                    java.util.logging.Logger.getLogger(TerminalGLPKSolver.class.getName()).log(Level.SEVERE, null, ex);
//                }
            }
        }
    }

    protected LinkedHashMap<String, Number> parseLpSolveOutput(ByteArrayOutputStream stdout, File output) throws LpSolverException {
        LinkedHashMap<String, Number> solutions = new LinkedHashMap<String, Number>();
        boolean startParsing = false;
        try {
            if (!output.exists()) {
                logger.error("The defined output file " + output.getAbsolutePath() + " does not exist!",
                        new IOException("Output file does not exist."));
            }
            FileInputStream inputStream = new FileInputStream(output);
            BufferedReader bReader = new BufferedReader(new InputStreamReader(new DataInputStream(inputStream)));
            String s;
            if (!isFeasible(output)) {
                throw new NoFeasibleSolutionException("This problem is infeasible.");
            } else {
                int varNameIndex = -1;
                int varUseIndex = -1;
                while ((s = bReader.readLine()) != null) {
                    if (startParsing && varNameIndex != -1 && varUseIndex != -1) {
                        if (s.startsWith("-")) {
                            continue;
                        }
                        String[] split = s.split("\\s+");
                        // extract column name
                        // extract value
                        String var;
                        Number num;

                        if (split.length >= 4) {
                            try {
                                var = split[varNameIndex].trim();
                                if (split[varUseIndex].trim().equals("*")) {
                                    varUseIndex++;
                                }
                                num = Double.parseDouble(split[varUseIndex].trim());
                                solutions.put(var, num);
                            } catch (NumberFormatException nfe) {
                                throw new LpSolverException("Unable to parse a number " + split[varUseIndex]);
                            }
                            solutions.put(var, num);
                        } else {
                            break;
                        }
                    }
                    startParsing = (startParsing) ? true : s.contains("Column name");
                    if (startParsing) {
                        // identify VarName and VarUse indices
                        String[] split = s.split("\\s+");
                        for (int i = 0; i < split.length; i++) {
                            if (split[i].trim().equals("Column")) {
                                varNameIndex = i;
                            } else if (split[i].trim().equals("Activity")) {
                                varUseIndex = i - 1;
                            }
                        }
                    }
                }
                
                inputStream.close();
                bReader.close();
            }
        } catch (IOException ex) {
            throw new LpSolverException("Failed to read the output : " + stdout.toString(), ex);
        } finally {
            if (delOutput) {
                FileUtils.deleteQuietly(output);
                output.deleteOnExit();
//                try {
//                    Runtime.getRuntime().exec("rm -f " + output);
//                } catch (IOException ex) {
//                    java.util.logging.Logger.getLogger(TerminalGLPKSolver.class.getName()).log(Level.SEVERE, null, ex);
//                }
            }
        }

        return solutions;
    }

    private boolean isFeasible(File output) throws IOException {
        FileInputStream inputStream = new FileInputStream(output);
        
        BufferedReader bReader = new BufferedReader(new InputStreamReader(new DataInputStream(inputStream)));
        
        String s;
        
        while ((s = bReader.readLine()) != null) {
        	
            if (s.contains("SOLUTION IS INFEASIBLE")) {
            	
                return false;
            }
        }
        
        return true;
    }

    @Override
    public LinkedHashMap<String, Number> solve() throws LpSolverException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
