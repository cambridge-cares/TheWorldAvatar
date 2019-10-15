package org.cam.ceb.como.enthalpy.calculator.filter.other;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */


import java.io.File;
import java.util.HashMap;
import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.glpk.MPSFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.glpk.TerminalGLPKSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.ISDReactionType;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.PoolModificationCalculator;
import org.cam.ceb.como.tools.objectpool.ObjectPool;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ProblematicSpeciesFilterTest {
    
    protected int numResults = 25;
    
    @Test
    @Ignore
    public void filterTest() throws Exception {
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        ObjectPool<Species> pool = new ObjectPool<Species>();
        pool.add(MockSpecies.getC2H4O());
        pool.add(MockSpecies.getC2H6());
        pool.add(MockSpecies.getC2H6O());
        pool.add(MockSpecies.getC3H6());
        pool.add(MockSpecies.getC3H8());
        pool.add(MockSpecies.getCH4());
        pool.add(MockSpecies.getCH4O());
        
        // modified invalid species which have to be filtered out!

        PoolModificationCalculator calc = new PoolModificationCalculator(numResults, solver, new MPSFormat(false, new ISDReactionType()), pool);
        calc.calculate(MockSpecies.getC2H4());
        HashMap<Species, ReactionList> list = (HashMap<Species, ReactionList>) calc.get();

        double exact = MockSpecies.getC2H4().getHf();
        for (Species s : list.keySet()) {
            for (int i = 0; i < list.get(s).size(); i++) {
                System.out.println(i + ": " + list.get(s).get(i).calculateHf());
                assert (Math.abs(exact - list.get(s).get(i).calculateHf()) < 10);
            }
        }
    }
}
