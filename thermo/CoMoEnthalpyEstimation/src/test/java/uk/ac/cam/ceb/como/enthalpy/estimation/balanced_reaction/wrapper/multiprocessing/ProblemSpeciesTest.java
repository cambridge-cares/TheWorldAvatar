/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.multiprocessing;


import java.io.File;
import java.io.FileNotFoundException;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import org.junit.Ignore;
import org.junit.Test;

import com.cmclinnovations.data.collections.ObjectPool;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.RadicalsFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.SpeciesFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.CSVParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;

/**
 *
 * @author pb556
 * 
 */
public class ProblemSpeciesTest {

    protected int numResults = 25;
    
    @Test
    @Ignore
    public void species1Test() throws FileNotFoundException, Exception, InterruptedException, Exception {
        // 696-86-6
        CSVParser parser = new SpeciesPoolParser(new File("test_data/csv/issues/pool_hydrocarbons_696-86-6.csv"));
        parser.parse();

        Collection<Species> ref = parser.getRefSpecies();
        Collection<Species> soi = parser.getSpeciesOfInterest();
        SpeciesFilter f = new RadicalsFilter(0, 1);
        f.set(ref);
        f.filter();
        ref = f.getValidSpecies();
        Collections.shuffle((List<Species>) ref);

        ObjectPool refPool = new ObjectPool();
        
        for (Species s : ref) {
        	
            refPool.add(s);
            
        }

//        Model m = new Model();
//        MPService service = new MPService(m, 1);
//        MPManager manager = new MPManager(service);

        for (Species s : soi) {
            LPSolver solver = new TerminalGLPKSolver(true, true);
            solver.setDirectory(new File("test_data/"));
            PoolModificationCalculator calc = new PoolModificationCalculator(numResults, solver, new MPSFormat(false, new ISDReactionType()), refPool);
            calc.calculate(s);
            HashMap<Species, ReactionList> list = (HashMap<Species, ReactionList>) calc.get();

            double exact = MockSpecies.getC2H4().getHf();
            
            for (Species sp : list.keySet()) {
                for (int i = 0; i < list.get(sp).size(); i++) {
                    System.out.println(i + ": " + list.get(sp).get(i).calculateHf());
                    System.out.println(list.get(sp).get(i).toString());
                    //assert (Math.abs(exact - list.get(sp).get(i).calculateHf()) < 10);
                }
            }

//            ObjectPool clone = SolverHelper.clone(refPool);
//            clone.validateAll();
//            clone.invalidate(s);
//            System.out.println("Processing species: " + s.getRef());
//            if (!clone.getInvalidatedObjects().isEmpty()) {
//                manager.addJob(s, clone, 20, 1, new TerminalGLPKSolver(true, true), new ISDMPSDoubleFormat());
//            }
        }
//        manager.startService();
//        while (true) {
//            Thread.sleep(100000);
//        }
    }
}
