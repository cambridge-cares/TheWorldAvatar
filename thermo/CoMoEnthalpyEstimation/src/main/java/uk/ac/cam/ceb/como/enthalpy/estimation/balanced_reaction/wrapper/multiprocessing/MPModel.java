/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.multiprocessing;

import java.util.concurrent.LinkedBlockingQueue;

/**
 *
 * @author pb556
 */
public class MPModel {
    
    protected volatile LinkedBlockingQueue<MPJob> inQueue;
    protected volatile LinkedBlockingQueue<MPJob> processed;
    protected volatile boolean completed = false;
    
    public MPModel() {
        inQueue = new LinkedBlockingQueue<MPJob>();
        processed = new LinkedBlockingQueue<MPJob>();
    }
    
    public void put(MPJob job) throws InterruptedException {
        completed = false;
        inQueue.put(job);
    }
    
    public MPJob poll() {
        return inQueue.poll();
    }
    
    public void putResults(MPJob job) throws InterruptedException {
        processed.put(job);
    }
    
    public MPJob pollResults() {
        return processed.poll();
    }
    
    public void complete() {
        completed = true;
    }
    
    public boolean isCompleted() {
        return completed;
    }
}
