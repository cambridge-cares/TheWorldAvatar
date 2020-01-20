/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import java.io.File;
import java.util.Map;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LpSolverException;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class OutputParserTest {
    @Test
    public void outputParserTest() throws LpSolverException {
        TerminalGLPKSolver solver = new TerminalGLPKSolver(true, false);
        Map<String, Number> data = solver.parseLpSolveOutput(null, new File("test_data/glpk/output/simple_test.out"));
        assert (data.get("absV[0]").intValue() == 1);
        assert (data.get("absV[1]").intValue() == 1);
        assert (data.get("absV[2]").intValue() == 1);
        assert (data.get("absV[3]").intValue() == 1);
        assert (data.get("v[0]").intValue() == 1);
        assert (data.get("v[1]").intValue() == 1);
        assert (data.get("v[2]").intValue() == -1);
        assert (data.get("v[3]").intValue() == -1);
    }
}
