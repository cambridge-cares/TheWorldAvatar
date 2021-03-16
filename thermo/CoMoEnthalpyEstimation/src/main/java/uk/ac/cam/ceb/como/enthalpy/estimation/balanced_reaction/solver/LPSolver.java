/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.UUID;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.SystemUtils;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public abstract class LPSolver {

	protected File dir = SystemUtils.getUserHome();    
    protected long timeout = 60000;
    protected boolean delInput = true;
    protected boolean delOutput = true;
    private Logger logger = Logger.getLogger(getClass());

    public LPSolver() {
    }

    public LPSolver(long timeout) {
        this.timeout = timeout;
    }

    public LPSolver(boolean delInputFile, boolean delOutputFile) {
        delInput = delInputFile;
        delOutput = delOutputFile;
    }

    public LPSolver(long timeout, boolean delInputFile, boolean delOutputFile) {
        this.timeout = timeout;
        delInput = delInputFile;
        delOutput = delOutputFile;
    }

    public boolean isInputDeleted() {
        return delInput;
    }

    public boolean isOutputDeleted() {
        return delOutput;
    }

    public File getDirectory() {
        return dir;
    }

    public long getTimeout() {
        return timeout;
    }

    public void set(boolean delInputFile, boolean delOutputFile) {
        delInput = delInputFile;
        delOutput = delOutputFile;
    }

    public void setTimeout(long timeout) {
        this.timeout = timeout;
    }

    /**
     * Set home directory for copying the lp_solve binary to and storing the
     * calculation files. Directory is created if not exist. Default directory
     * is
     * <code>{user.home}/.gigatools/extra/lp_solver/</code>
     *
     * @param home a file to home directory
     */
    public void setDirectory(File dir) {
        this.dir = dir;
    }

    /**
     * Solve the problem given by an input as a String.
     *
     * @param lpInputString an input as a String
     * @return a map of solutions where keys are the variable names and values
     * are the results
     * @throws LpSolverException thrown if there is an io error from exceuting
     * lp_solve binary.
     * @throws NoFeasibleSolutionException throw if no io error nut no solution
     * was found.
     */
    public Map<String, Number> solve(String lpInputString) throws LpSolverException, NoFeasibleSolutionException {
        File tempFile = getTempFile();
        try {
            FileUtils.writeStringToFile(tempFile, lpInputString, "UTF-8");
        } catch (IOException ex) {
            throw new LpSolverException("Unable to write temporary lp_solve input to file system at " + tempFile.getPath(), ex);
        }
        Map<String, Number> sol = solve(tempFile);
        if (delInput) {
            FileUtils.deleteQuietly(tempFile);
//            try {
//                Runtime.getRuntime().exec("rm -f " + tempFile);
//            } catch (IOException ex) {
//                java.util.logging.Logger.getLogger(LPSolver.class.getName()).log(Level.SEVERE, null, ex);
//            }
        }
        return sol;
    }

    /**
     * Solve the problem given by an input as a File.
     *
     * @param lpInputFile an input as a File
     * @return a map of solutions where keys are the variable names and values
     * are the results
     * @throws LpSolverException thrown if there is an io error from exceuting
     * lp_solve binary.
     */
    public abstract Map<String, Number> solve(File lpInputFile) throws LpSolverException;

    public abstract Map<String, Number> solve() throws LpSolverException;

    protected File getTempFile() {
    	
        File tempDir = new File(dir,".temp/");
        
        if (!tempDir.exists()) {
        	
            if (!tempDir.mkdir()) {
            	
                logger.error("The directory " + tempDir + " could not be created!",
                		
                        new IOException("The directory " + tempDir + " could not be created!"));
            }
        }
        
        File tempFile = new File(dir, ".temp/" + UUID.randomUUID());
        
        return tempFile;
    }
}