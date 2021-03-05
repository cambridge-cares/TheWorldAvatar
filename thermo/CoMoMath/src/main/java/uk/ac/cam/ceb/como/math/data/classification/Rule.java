/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data.classification;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author pb556
 */
public abstract class Rule<T> {
    
    protected T obj;
    
    public Rule() {
    }
    
    public Rule(T obj) {
        this.obj = obj;
    }
    
    public T get() {
        return obj;
    }
    
    public void set(T obj) {
        this.obj = obj;
    }
    
    public boolean apply(Object...additionalParameters) throws Exception {
        return apply(get());
    }
    
    public abstract boolean apply(T obj, Object...additionalParameters) throws Exception; 
    
    public Map<T, Boolean> apply(Collection<T> obj, Object...additionalParameters) throws Exception {
        Map<T, Boolean> results = new HashMap<T, Boolean>();
        for (T p : obj) {
            results.put(p, apply(p, additionalParameters));
        }
        return results;
    }
}
