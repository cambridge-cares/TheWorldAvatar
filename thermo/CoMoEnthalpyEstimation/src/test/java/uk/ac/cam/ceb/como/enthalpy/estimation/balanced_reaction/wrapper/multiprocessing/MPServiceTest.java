/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.multiprocessing;

import java.io.File;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.junit.Test;

import com.cmclinnovations.data.collections.ObjectPool;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.RadicalsFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.SpeciesFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.CSVParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.SolverHelper;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 *
 * @author pb556
 */

public class MPServiceTest {
    
    @Test
//    @Ignore
    public void calculatorTest() throws Exception {
        CSVParser parser = new SpeciesPoolParser(new File("test_data/csv/pool_hydrocarbons.csv"));
        parser.parse();

        Collection<Species> ref = parser.getRefSpecies();
        SpeciesFilter f = new RadicalsFilter(0, 1);
        f.set(ref);
        f.filter();
        ref = f.getValidSpecies();
        Collections.shuffle((List<Species>) ref);

        ObjectPool refPool = new ObjectPool();
        for (Species s : ref) {
            refPool.add(s);
        }
        
        MPModel m = new MPModel();
        MPService service = new MPService(m, 20);
        MPManager manager = new MPManager(service);
        
        for (Species soi : ref) {
            ObjectPool clone = SolverHelper.clone(refPool);
            clone.validateAll();
            clone.invalidate(soi);
            System.out.println("Processing species: " + soi.getRef());
            if (!clone.getInvalidatedObjects().isEmpty()) {
                manager.addJob(soi, clone, 20, 10, new TerminalGLPKSolver(true, true), new MPSFormat(false, new ISDReactionType()));
            }
        }
        manager.startService();
        while(true) {
            Thread.sleep(100000);
        }
    }
}
