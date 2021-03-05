/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.multiprocessing.simple;

/**
 *
 * @author pb556
 */
public abstract class Consumer extends Thread {
    
    protected Broker broker;
    protected String id;
    
    public Consumer(String id) {
        this.id = id;
    }
    
    public Consumer(String id, Broker broker) {
        this.id = id;
        this.broker = broker;
    }
    
    public void set(Broker broker) {
        this.broker = broker;
    }
    
    public String getObjId() {
        return id;
    }
    
    public abstract void startConsumption();
    public abstract void finishConsumption();
    public abstract void pauseConsumption();
    public abstract void pursueConsumption();
}
