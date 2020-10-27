/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.paper.enthalpy.utils;

import com.cmclinnovations.data.collections.ObjectPool;
import java.util.Collection;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.HDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.HHDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 *
 * @author pb556
 */
public class EvaluationUtils {

    public static ObjectPool<Species> getPool(Collection<Species> pool, boolean validatedSpeciesOnly) {
        ObjectPool<Species> newPool = new ObjectPool<>();
        for (Species s : pool) {
            newPool.add(s);
        }
        newPool.validateAll();
        return newPool;
    }

    public static ObjectPool<Species> getPool(Collection<Species> pool, Species targetSpecies, boolean validatedSpeciesOnly) {
        ObjectPool<Species> newPool = new ObjectPool<>();
        for (Species s : pool) {
            if (s.equals(targetSpecies, true)) {
                continue;
            }
            newPool.add(s);
        }
        newPool.validateAll();
        return newPool;
    }
    
    /**
     * @author NK510 (caresssd@hermes.cam.ac.uk)
     * @param inputJsonKey
     * @return reactuib type depends on input json key value.
     */
    public static ReactionType getReactionType(String inputJsonKey) {    	
    	
    	ReactionType reactionType = null;
    	
    	if(inputJsonKey.equalsIgnoreCase("ISG")) {
    		
    		ISGReactionType isgReactionType = new ISGReactionType(true);
    		
    		return isgReactionType;
    		
    	} else {
    		
    		if (inputJsonKey.equalsIgnoreCase("ISD")){
    			
    			ISDReactionType isdReactionType = new ISDReactionType();
    			
    			return isdReactionType;
    			
    		}
    	} if(inputJsonKey.equalsIgnoreCase("HD")) {
    		
    		HDReactionType hdReactionType = new HDReactionType();
    		
    		return hdReactionType;
    	
    	}else {
    		
    		if(inputJsonKey.equalsIgnoreCase("HHD")) {
    			
    			HHDReactionType hhdReactionType = new HHDReactionType();
    			
    			return hhdReactionType;
    		}
    	}
    	
    	return reactionType;
    }
    /**
     * @author NK510 (caresssd@hermes.cam.ac.uk)
     * @param inputJsonKey
     * @return the number of runs used in global cross validation algorithm
     */
    public static int[] getCtrRuns(int inputJsonKey) {
    	int[] ctrRuns = new int[] {inputJsonKey};
    	return ctrRuns;
    }
    /**
     * @author NK510 (caresssd@hermes.cam.ac.uk)
     * @param inputJsonKey
     * @return the number of reactions to be generated in global cross validation algorithm.
     */
    public static int[] getCtrRes(int inputJsonKey) {
    	int[] ctrRes = new int[] {inputJsonKey};
    	return ctrRes;
    }
    
    /**
     * @author NK510 (caresssd@hermes.cam.ac.uk)
     * @param inputJsonKey
     * @return the number of radicals used in global cross validation algorithm.
     */
    public static int[] getCtrRadicals(int inputJsonKey) {
    	int[] ctrRadicals = new int[] {inputJsonKey};
    	return ctrRadicals;
    }
}