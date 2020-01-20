/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.paper.enthalpy.utils;

import com.cmclinnovations.data.collections.ObjectPool;
import java.util.Collection;
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
}
