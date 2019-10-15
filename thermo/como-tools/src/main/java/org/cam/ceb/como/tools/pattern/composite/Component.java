/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.pattern.composite;

/**
 *
 * @author pb556
 */
public abstract class Component {
    
    protected Object obj = null;
    
    public void set(Object obj) {
        this.obj = obj;
    }
    
    public Object get() {
        return this.obj;
    }
    
    public abstract void execute();
}
