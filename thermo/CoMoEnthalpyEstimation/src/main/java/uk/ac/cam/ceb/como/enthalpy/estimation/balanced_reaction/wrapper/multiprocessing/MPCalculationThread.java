/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.multiprocessing;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LpSolverException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.NoFeasibleSolutionException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.Solver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.SolverHelper;
import com.cmclinnovations.data.collections.ObjectPool;

/**
 *
 * @author pb556
 */
public class MPCalculationThread extends Thread {

    // does everything
    protected String threadId = null;
    protected boolean started = false;
    protected boolean finished = false;
    protected MPJob job = null;
    protected volatile MPModel model = null;

    public MPCalculationThread(String id, MPModel model) {
        threadId = id;
        this.model = model;
    }

    public void set(MPJob job) {
        this.job = job;
        started = false;
        finished = false;
    }

    public boolean isFinished() {
        return finished;
    }

    @Override
    public void run() {
        while (!model.inQueue.isEmpty()) {
            job = model.poll();
            System.out.println("Process species " + job.getId() + "...");
            if (job != null) {
                finished = false;
                int ctr = 0;
                while (!finished || ctr > 100) {
                    ctr++;
                    try {
                        if (job == null) {
                            break;
                        }
                        MPTask t = consume();
                        if (t != null) {
                            Reaction r = processConsumption(t);
                            if (r == null) {
                                //t.setResult(r, Task.Status.UNSOLVABLE);
                            } else {
                                t.setResult(r, MPTask.Status.SOLVED);
                            }
                            if (r != null) {
                                job.putResult(t);
                                job.putProcessedResult(t);
                                if (job.procResQueue.size() > job.numSolutions || job.resQueue.isEmpty()) {
                                    finished = true;
                                } else {
                                    produce();
                                }
                            }
                        }

                        //System.out.println("Consumed: " + );
                        //Thread.sleep(50);
                    } catch (InterruptedException ex) {
                        System.out.println("Interrupted calc..." + threadId);
                    }

                }
                try {
                    model.processed.put(job);
                } catch (InterruptedException ex) {
                    Logger.getLogger(MPCalculationThread.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
            System.out.println("Finished species " + job.getId() + "...");
        }
        System.out.println(threadId + " has been terminated! No more jobs available.");
    }

    private MPTask consume() throws InterruptedException {
//        synchronized (job) {
//            while (job.isPaused() || job.inQueue.isEmpty()) {
//                //synchronized (list) {
//                System.out.println("Queue is empty " + Thread.currentThread().getName() + " " + threadId + " is waiting"); // + list.size());
//                job.wait();
//            }
//        }
        return job.pollTask();
    }

    private Reaction processConsumption(MPTask t) throws InterruptedException {
        Solver enthalpySolver = t.getSolver();
        for (Object s : t.getPool().getValidatedObjects()) {
            enthalpySolver.addReferenceSpecies((Species) s);
        }
        try {
            Reaction r = enthalpySolver.solve(t.getTargetSpecies());
            System.out.println(threadId + " (" + job.getId() + ") " + r.calculateHf());
            return r;
        } catch (NoFeasibleSolutionException ex) {
            System.out.println("Interrupted calc..." + threadId);
        } catch (LpSolverException ex) {
            System.out.println("Interrupted calc..." + threadId);
        }
        return null;
    }

    private void produce() throws InterruptedException {
        try {
            MPTask t = job.pollResult(); // Get a task from the queue.
            if (t != null) {
                processProduction(t);
            }
            //list.current().putProcessedResult(t);
        } catch (Exception e) {
            System.out.println("POLL!");
        }
    }

    public boolean processProduction(MPTask t) throws InterruptedException {
        //System.out.println(id + " calculate...");
        Species targetSpecies = t.getTargetSpecies();
        ObjectPool pool = t.getPool();
        if (t.getResult() == null) {
            return false;
        }
        if (job.isFinished()) {
            return false;
        }
        //task.run();  // Execute the task;
        Reaction reaction = t.getResult();
        List<Species> speciesList = new ArrayList<Species>();
        for (Species s : reaction.getProducts().keySet()) {
            if (!s.equals(targetSpecies, false)) {
                speciesList.add(s);
            }
        }
        for (Species s : reaction.getReactants().keySet()) {
            if (!s.equals(targetSpecies, false)) {
                speciesList.add(s);
            }
        }
        List combs = any(speciesList.size(), speciesList);
        for (int i = 0; i < combs.size(); i++) {
            ObjectPool clonedPool;
            try {
                clonedPool = SolverHelper.clone(pool);
            } catch (OutOfMemoryError e) {
                System.gc();
                try {
                    clonedPool = SolverHelper.clone(pool);
                } catch (OutOfMemoryError e2) {
                    continue;
                }
            }
            List<Species> l = (List<Species>) combs.get(i);
            for (Species s : l) {
                clonedPool.invalidate(s);
            }
            job.putTask(new MPTask(t.getLPSolver(), t.getLPFormat(), targetSpecies, clonedPool));
        }
        if (combs.isEmpty()) {
            return false;
        }
        return true;
    }

    // returns multiple possible reactions for the same target species
    private List<List> any(final int n, final List list) {
        if (n < 0) {
            throw new RuntimeException("Choose Any less than zero : n = " + n);
        }
        if (n > list.size()) {
            throw new RuntimeException("Choose Any larger than the size of the list : n = " + n);
        }
        List<List> all = new ArrayList<List>();
        if (n == 0) {
            List sublist = new ArrayList();
            all.add(sublist);
            return all;
        } else if (n == 1) {
            for (Object l : list) {
                List sublist = new ArrayList();
                sublist.add(l);
                all.add(sublist);
            }
            return all;
        } else {
            for (int i = 0; i < list.size(); i++) {
                List rest = new ArrayList(list);
                Object left = list.get(i);
                List<List> rlist = any(n - 1, rest);
                for (List l : rlist) {
                    l.add(0, left);
                    all.add(l);
                }
            }
            return all;
        }
    }
}
