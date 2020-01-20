/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.statistics.validation;

import com.cmclinnovations.data.collections.ObjectPool;
import java.util.Map;

/**
 *
 * @author pb556
 */
public class CrossValidationResult {
    
    protected ObjectPool refPool = null;
    protected Map results = null;
    
    public void set(ObjectPool pool) {
        refPool = pool;
    }
    
    public void set(Map results) {
        this.results = results;
    }
    
    public ObjectPool getPool() {
        return refPool;
    }
    
    public Map getResults() {
        return results;
    }
}
