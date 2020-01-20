/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.validation;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.validation.SpeciesPoolValidation;
import java.io.File;
import java.io.FileNotFoundException;

import java.util.Collection;
import java.util.Map;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.RadicalsFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionListWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import com.cmclinnovations.data.collections.ObjectPool;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */

public class SpeciesPoolValidationTest {
    
    @Test
    @Ignore
    public void csvTest() throws FileNotFoundException, Exception, Exception {
    	
        //String path = "W:\\Data\\TTIP\\enthalpy\\poster\\pool_153.csv";
        //String dest = "W:\\Data\\TTIP\\enthalpy\\poster\\pool_153_validated_25_10_radicals1_runs1_res5.csv";
        //String destRList = "W:\\Data\\TTIP\\enthalpy\\poster\\rlist_153_validated_25_10_radicals1_runs1_res5.rct";
                
        String path = "W:\\Data\\TTIP\\NIST\\isd\\pool_hydrocarbons_m.csv";
        String dest = "W:\\Data\\TTIP\\NIST\\isd\\pool_hydrocarbons_validated_25_10_radicals1_runs1_res10.csv";
        String destRList = "W:\\Data\\TTIP\\NIST\\isd\\rlist_hydrocarbons_validated_25_10_radicals1_runs1_res10.rct";
        
        SpeciesPoolParser refParser = new SpeciesPoolParser(new File(path));
        refParser.parse();
        Collection<Species> ref = refParser.getRefSpecies();
        
        RadicalsFilter filter = new RadicalsFilter(0, 1);

        SpeciesPoolValidation validation = new SpeciesPoolValidation(
                getPool(filter.filter(ref)), 
                new TerminalGLPKSolver(true, true), 
                new MPSFormat(false, new ISDReactionType()), 
                25.0, 
                0.10);
        
        validation.setNumberOfResults(10);
        validation.setNumberOfRuns(1);
        validation.validate();
        ObjectPool<Species> selected = validation.getSpeciesPool();
        Map<Species, Collection<ReactionList>> details = validation.getDetailedResults();   
        
        SpeciesPoolWriter writer = new SpeciesPoolWriter(new File(dest));
        writer.set(selected.getValidatedObjects(), false);
        writer.write();
        
        ReactionList rList = new ReactionList();
        for (Species s : details.keySet()) {
            for (ReactionList l : details.get(s)) {
                rList.addAll(l);
            }
        }
        
        ReactionListWriter rListWriter = new ReactionListWriter(new File(destRList));
        rListWriter.set(rList);
        rListWriter.overwrite(true);
        rListWriter.write();
        
        System.out.println();
    }
    
    public ObjectPool<Species> getPool(Collection<Species> sp) {
        ObjectPool<Species> pool = new ObjectPool<Species>();
        pool.addAll(sp);
        pool.validateAll();
        return pool;
    }
}
