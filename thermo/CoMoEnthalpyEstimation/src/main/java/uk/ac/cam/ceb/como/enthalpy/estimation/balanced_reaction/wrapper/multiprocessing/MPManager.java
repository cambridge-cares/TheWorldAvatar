/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.multiprocessing;

import java.util.Collection;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import com.cmclinnovations.data.collections.ObjectPool;

/**
 *
 * @author pb556
 */
public class MPManager extends Thread {
    // has a certain number of services
    
    protected volatile MPService service = null;
    protected volatile boolean serviceStarted = false;
    
    // add jobs, execute service and maintain service
    public MPManager(MPService service) {
        this.service = service;
    }
    
    public void addJob(Species targetSpecies, Collection pool, int numSolutions, int queueCapacity, LPSolver solver, LPFormat format) throws InterruptedException {
        ObjectPool p = new ObjectPool();
        p.addAll(pool);
        p.validateAll();
        addJob(targetSpecies, p, numSolutions, queueCapacity, solver, format);
    }
    
    public void addJob(Species targetSpecies, ObjectPool pool, int numSolutions, int queueCapacity, LPSolver solver, LPFormat format) throws InterruptedException {
        // add a new initial job which should be used by the consumer and taken by the producer after the initial result
        // could be obtained by the defined solver

        // wait until job is completely executed
        MPJob job = new MPJob(targetSpecies.getRef(), numSolutions);
        job.putTask(new MPTask(solver, format, targetSpecies, pool));
        addJob(job);
    }
    
    public void addJob(MPJob job) throws InterruptedException {
        // add a new initial job which should be used by the consumer and taken by the producer after the initial result
        // could be obtained by the defined solver

        // wait until job is completely executed
        service.model.put(job);
        //service.getBrokerList().next();
    }
    
    public void startService() {
        if (!serviceStarted) {
            serviceStarted = true;
            service.start();
        }
    }
    
    public void stopService() throws InterruptedException {
        if (serviceStarted) {
            serviceStarted = false;
            service.stop();
        }
    }
    
    public boolean isProcessing() {
        return service.isProcessing();
    }
}
