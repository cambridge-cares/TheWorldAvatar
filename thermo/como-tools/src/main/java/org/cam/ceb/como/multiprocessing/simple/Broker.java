/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.multiprocessing.simple;

import java.util.concurrent.ArrayBlockingQueue;

/**
 *
 * @author pb556
 */
public class Broker<T> {
    
    // contains the tasks that need to be processed
    protected volatile ArrayBlockingQueue<T> taskQueue;
    
    protected volatile Status productionStatus = Status.PAUSE;
    protected volatile Status consumptionStatus = Status.PAUSE;
    
    public enum Status {
        RUN, STOP, PAUSE
    }
            
    public Broker(int capacity) {
         taskQueue = new ArrayBlockingQueue<T>(capacity);
    }
    
    public synchronized void setProductionStatus(Status s) {
        productionStatus = s;
    }
    
    public synchronized void setConsumptionStatus(Status s) {
        consumptionStatus = s;
    }
    
    public synchronized Status getProductionStatus() {
        return productionStatus;
    }
    
    public synchronized Status getConsumptionStatus() {
        return consumptionStatus;
    }
    
    public synchronized void put(T task) throws InterruptedException {
        taskQueue.put(task);
    }
    
    public synchronized T poll() throws InterruptedException {
        return taskQueue.poll();
    }
}