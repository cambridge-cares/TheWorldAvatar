/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.multiprocessing;

import java.util.concurrent.BlockingQueue;

/**
 *
 * @author pb556
 */
public abstract class Consumer extends Thread {
    
    protected volatile BlockingQueue taskQueue;
    protected volatile Status status = Status.RUN;
    protected long execTime = -1;
    
    public enum Status {

        RUN, PAUSE, ABORT
    }
    
    public Consumer(BlockingQueue queue) {
        super();
        taskQueue = queue;
    }
    
    public long getExecutionTime() {
        return execTime;
    }
    
    synchronized private void checkStatus() {
        while (status == Status.PAUSE) {
            try {
                paused();
                wait();
            } catch (InterruptedException e) {}
        }
        if (status == Status.RUN) {
            continued();
        }
        if (status == Status.ABORT) {
            aborted();
        }
    }

    @Override
    public void run() {
        long startTime = System.nanoTime();
        try {
            while (status != Status.ABORT) {
                checkStatus();
                Runnable task = (Runnable) taskQueue.poll(); // Get a task from the queue.
                if (task == null) {
                    break; // (because the queue is empty)
                }
                task.run();  // Execute the task;
            }
        } finally {
            finished(); // Records fact that this thread has terminated.
            execTime = System.nanoTime() - startTime;
        }
    }

    public abstract void finished();

    public abstract void paused();

    public abstract void aborted();

    public abstract void continued();
}
