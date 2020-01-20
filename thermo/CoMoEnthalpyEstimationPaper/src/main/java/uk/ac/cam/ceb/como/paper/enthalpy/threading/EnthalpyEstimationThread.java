/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.paper.enthalpy.threading;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.Callable;

import com.cmclinnovations.data.collections.ObjectPool;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.ObjectPoolCalculator;

/**
 *
 * @author pb556
 * 
 */
public class EnthalpyEstimationThread implements Callable<Map<Species, Collection<ReactionList>>> {

    private ObjectPoolCalculator c = null;
    private Species s = null;
    private ObjectPool<Species> pool = null;

    public EnthalpyEstimationThread(ObjectPoolCalculator c, Species s, ObjectPool<Species> pool) {
        this.c = c;
        this.s = s;
        this.pool = pool;
    }
    
    public ObjectPoolCalculator getCalculator() {
    	
        return c;
        
    }

    @Override
    public Map<Species, Collection<ReactionList>> call() throws Exception {
    	
        c.set(pool);
        
        try {
        	
            c.calculate(s);
            
            return (Map<Species, Collection<ReactionList>>) c.get();
            
        } catch (OutOfMemoryError e) {
        	
            System.gc();
        }
        
        return null;
    }
}
