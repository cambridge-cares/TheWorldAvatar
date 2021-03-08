/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.statistics.validation;

import com.cmclinnovations.data.collections.ObjectPool;
import java.util.Collection;

/**
 *
 * @author pb556
 */
public abstract class CrossValidation {
    
    protected ObjectPool pool;
    
    public CrossValidation(ObjectPool pool) {
        this.pool = pool;
    }
    
    public abstract void validate();
    
    public abstract Collection<CrossValidationResult> getValidationResults();
    
}
