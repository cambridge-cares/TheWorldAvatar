/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.validation;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

import org.apache.log4j.Logger;

import com.cmclinnovations.data.collections.ObjectPool;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.ReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.SolverHelper;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.multiprocessing.MPManager;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.multiprocessing.MPModel;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.multiprocessing.MPService;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.ObjectPoolCalculator;

/**
 *
 * @author pb556
 */
public class MPLeaveOneOutCrossValidation implements Runnable {

    protected ObjectPoolCalculator calculator = null;
    protected ReactionSelector selector = null;
    protected HashMap<Species, Double> results = null;
    protected ObjectPool pool = null;
    private Logger logger = Logger.getLogger(getClass());

    public MPLeaveOneOutCrossValidation(ObjectPool<Species> pool, ObjectPoolCalculator calculator, ReactionSelector selector) {
    	
        this.pool = pool;
        this.calculator = calculator;
        this.selector = selector;
    }

    public MPLeaveOneOutCrossValidation(ObjectPool<Species> pool, ObjectPoolCalculator calculator) {
    	
        this.pool = pool;
        this.calculator = calculator;
    }

    public void validate() {
    	
        this.run();
    }

    public Map<Species, Double> getValidationResults() {
    	
        return results;
    }

    @Override
    public void run() {
    	
        results = new HashMap<Species, Double>();
        ObjectPool orig = new ObjectPool();
        orig.addAll(pool.getValidatedObjects());
        HashMap<Species, Object> res = new HashMap<Species, Object>();

        MPModel m = new MPModel();
        MPService service = new MPService(m, 4);
        MPManager manager = new MPManager(service);

        for (Object soi : orig.getValidatedObjects()) {
            ObjectPool clone = SolverHelper.clone(orig);
            Species selSOI = null;
            for (Object x : clone.getInvalidatedObjects()) {
                Species o = (Species) x;
                if (o.equals((Species) soi, true)) {
                    selSOI = (Species) o;
                    break;
                }
            }
            if (selSOI == null) {
                for (Object x : clone.getValidatedObjects()) {
                    Species o = (Species) x;
                    if (o.equals((Species) soi, true)) {
                        selSOI = (Species) o;
                        break;
                    }
                }
            }

            try {
                if (selSOI != null) {
                    clone.invalidate(selSOI);
                    System.out.println("Processing species: " + selSOI.getRef());
                    if (!clone.getInvalidatedObjects().isEmpty()) {
                        manager.addJob(selSOI, clone, 20, 10, new TerminalGLPKSolver(true, true), new MPSFormat(false, new ISDReactionType()));
                    }
                } else {
                    clone.invalidate((Species) soi);
                    System.out.println("Processing species: " + ((Species) soi).getRef());
                    if (!clone.getInvalidatedObjects().isEmpty()) {
                        manager.addJob((Species) soi, clone, 20, 10, new TerminalGLPKSolver(true, true), new MPSFormat(false, new ISDReactionType()));
                    }
                }
            } catch (Exception e) {
            }            
        }
        manager.startService();
        while(manager.isProcessing()) {
            try {
                Thread.sleep(100000);
            } catch (InterruptedException ex) {
                java.util.logging.Logger.getLogger(MPLeaveOneOutCrossValidation.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        System.out.println("FINISH!!!");
    }
}