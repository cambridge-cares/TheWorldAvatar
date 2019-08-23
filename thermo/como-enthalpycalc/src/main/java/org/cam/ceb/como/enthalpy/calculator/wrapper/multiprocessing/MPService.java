/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.multiprocessing;

import java.util.ArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/**
 *
 * @author pb556
 */
public class MPService {
    
    protected int num = 1;
    protected volatile MPModel model;
    protected ExecutorService threadPool = null;
    protected ArrayList<MPCalculationThread> threads = new ArrayList<MPCalculationThread>();
    
    public MPService(MPModel model, int num) {
        this.model = model;
        this.num = num;
    }
    
    public void start() {
        threadPool = Executors.newFixedThreadPool(num);
        for (int i = 0; i < num; i++) {
            MPCalculationThread t = new MPCalculationThread("Thread" + i, model);
            threadPool.execute(t);
        }
    }
    
    public void stop() throws InterruptedException {
        threadPool.shutdown();
        if (!threadPool.awaitTermination(600, TimeUnit.SECONDS)) {
            threadPool.shutdownNow();
        }
    }
    
    public boolean isProcessing() {
        if (threadPool == null) {
            return false;
        }
        if (threadPool.isShutdown()) {
            return false;
        }
        if (model.inQueue.isEmpty()) {
            for (MPCalculationThread t : threads) {
                if (!t.isFinished()) {
                    System.out.println(t.getName() + " is busy!");
                    return true;
                }
            }
            return false;
        }
        System.out.println("Queue not empty " + model.inQueue.size() + " - processed: " + model.processed.size());
        return true;
    }
}
