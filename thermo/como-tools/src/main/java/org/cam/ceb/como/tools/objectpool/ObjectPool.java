/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.objectpool;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

/**
 *
 * @author pb556
 */
public class ObjectPool<T> {

    private HashMap<T, Boolean> locked;

    public ObjectPool() {
        locked = new HashMap<T, Boolean>();
    }

    public void add(T obj) {
        locked.put(obj, Boolean.FALSE);
    }
    
    public void addValidated(T obj) {
        this.locked.put(obj, Boolean.FALSE);
    }
    
    public void addInvalidated(T obj) {
        this.locked.put(obj, Boolean.TRUE);
    }
    
    public void add(T obj, Boolean locked) {
        this.locked.put(obj, locked);
    }
    
    public void addAll(Collection<T> obj) {
        for (T o : obj) {
            add(o);
        }
    }
    
    public void remove(T obj) {
        locked.remove(obj);
    }
    
    public void removeAll(Collection<T> obj) {
        for (T o : obj) {
            remove(o);
        }
    }
    
    public void clear() {
        locked = new HashMap<T, Boolean>();
    }
    
    public void invalidate(T obj) {
        locked.put(obj, Boolean.TRUE);
    }
    
    public void validate(T obj) {
        locked.put(obj, Boolean.FALSE);
    }
    
    public void validateAll() {
        for (T obj : locked.keySet()) {
            validate(obj);
        }
    }
    
    public void invalidateAll() {
        for (T obj : locked.keySet()) {
            invalidate(obj);
        }
    }
    
    public Collection<T> getValidatedObjects() {
        ArrayList<T> objs = new ArrayList<T>();
        for (T obj : locked.keySet()) {
            if (!locked.get(obj)) {
                objs.add(obj);
            }
        }
        return objs;
    }
    
    public Collection<T> getInvalidatedObjects() {
        ArrayList<T> objs = new ArrayList<T>();
        for (T obj : locked.keySet()) {
            if (locked.get(obj)) {
                objs.add(obj);
            }
        }
        return objs;
    }
}
