/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.multiprocessing.simple;

/**
 *
 * @author pb556
 */
public abstract class Producer extends Thread {
    
    protected Broker broker;
    protected String id;
    
    public Producer(String id) {
        this.id = id;
    }
    
    public Producer(String id, Broker broker) {
        this.id = id;
        this.broker = broker;
    }
    
    public void set(Broker broker) {
        this.broker = broker;
    }
    
    public String getObjId() {
        return id;
    }
    
    public abstract void startProduction();
    public abstract void finishProduction();
    public abstract void pauseProduction();
    public abstract void pursueProduction();
}
