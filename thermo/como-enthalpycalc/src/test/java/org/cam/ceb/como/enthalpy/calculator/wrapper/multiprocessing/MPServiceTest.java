/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.multiprocessing;

import java.io.File;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.filter.SpeciesFilter;
import org.cam.ceb.como.enthalpy.calculator.filter.RadicalsFilter;
import org.cam.ceb.como.enthalpy.calculator.io.pool.CSVParser;
import org.cam.ceb.como.enthalpy.calculator.io.pool.SpeciesPoolParser;
import org.cam.ceb.como.enthalpy.calculator.solver.SolverHelper;
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
public class MPServiceTest {
    
    @Test
    @Ignore
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
