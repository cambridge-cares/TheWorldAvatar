package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.other;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import java.io.File;
import java.util.HashMap;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
import com.cmclinnovations.data.collections.ObjectPool;
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