/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.multiprocessing;

import java.util.concurrent.LinkedTransferQueue;

/**
 *
 * @author pb556
 */
public class MPJob {
    // each job has a number of tasks
    // contains the tasks that need to be processed
    protected final String id;
    protected volatile LinkedTransferQueue<MPTask> inQueue;
    protected volatile LinkedTransferQueue<MPTask> resQueue;
    protected volatile LinkedTransferQueue<MPTask> procResQueue;
    protected volatile Status status = Status.NOT_STARTED;
    protected volatile int numSolutions = 1;

    public enum Status {

        RUNNING, FINISHED, PAUSED, NOT_STARTED, STOPPED, INCOMPLETE
    }

    public MPJob(String id, int numSolutions) {
        inQueue = new LinkedTransferQueue<MPTask>();
        resQueue = new LinkedTransferQueue<MPTask>();
        procResQueue = new LinkedTransferQueue<MPTask>();
        this.numSolutions = numSolutions;
        this.id = id;
    }
    
    public String getId() {
        return id;
    }

    public void putTask(MPTask task) throws InterruptedException {
        inQueue.put(task);
    }

    public synchronized MPTask pollTask() throws InterruptedException {
        return inQueue.poll();
    }

    public void putResult(MPTask task) throws InterruptedException {
        resQueue.put(task);
    }

    public synchronized MPTask pollResult() throws InterruptedException {
        return resQueue.poll();
    }
    
    public void putProcessedResult(MPTask task) throws InterruptedException {
        procResQueue.put(task);
    }

    public synchronized MPTask pollProcessedResult() throws InterruptedException {
        return procResQueue.poll();
    }

    public synchronized void clear() {
        inQueue.clear();
        resQueue.clear();
    }
    
    public int getNumberOfSolutions() {
        return numSolutions;
    }
    
    public synchronized int getNumberOfResults() {
        return procResQueue.size();
    }

    public synchronized void set(Status status) {
        this.status = status;
    }

    public synchronized boolean isFinished() {
        //System.out.println("# of processed results: " + getNumberOfResults());
        if (getNumberOfResults() > numSolutions) {
            status = Status.FINISHED;
        }
        return status == Status.FINISHED;
    }

    public synchronized boolean isProgress() {
        return status == Status.RUNNING;
    }

    public synchronized boolean isWaiting() {
        return status == Status.NOT_STARTED;
    }

    public synchronized boolean isPaused() {
        return status == Status.PAUSED;
    }
}
