/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.multiprocessing;

import java.util.List;

/**
 *
 * @author pb556
 */
public abstract class Producer extends Thread {
    
    protected ThreadPool pool;
    
    
    public Producer(ThreadPool pool) {
        this.pool = pool;
    }
    
    public ThreadPool getThreadPool() {
        return pool;
    }
    
    public void set(ThreadPool pool) {
        this.pool = pool;
    }
    
    public abstract List<Thread> getAllThreads();
    
    public abstract void produce();
    
    public abstract Thread next();
    
    public abstract void finishProduction();
}
