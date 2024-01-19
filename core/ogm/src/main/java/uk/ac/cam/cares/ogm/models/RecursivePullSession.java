package uk.ac.cam.cares.ogm.models;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

/**
 * {@link RecursivePullSession} operates in a loop where it calls {@link ModelContext#pullAll(Model)} on all items in
 * its working queue, causing invocations of {@link ModelContext#getModel(Class, String)} by {@link FieldInterface}s,
 * which are reported to {@link RecursivePullSession#queue(Model)}, where they are added to the pending queue. The
 * pending queue is then moved to the working queue and the process repeats.
 * <p>
 * Each round of pulling retrieves nodes one step away from the previous round's nodes, i.e. an additional degree of
 * separation from the origin provided in the session construction. Repeat visits are prevented by tracking the set of
 * traversed nodes. The number of rounds of pulling thus defines the maximum separation radius from the origin to
 * retrieve, which is our interpretation of {@code recursionRadius} for a non-acyclic graph. The breadth-first
 * strategy is necessary to prevent paths from cutting each other off.
 * If execute method is called from outside ModelContext, it is required to set the currentpullsession field of ModelContext
 * before the call to execute function to update the queue and traverediris during recursion
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
public class RecursivePullSession {

    private Queue<Model> pendingPullQueue = new LinkedList<>();
    private Queue<Model> workingPullQueue = new LinkedList<>();
    private final Set<String> traversedIris = new HashSet<>();
    private int remainingDegreesOfSeparation;
    public ModelContext context;

    private final boolean partial;
    private final String[] fieldNames;

    public RecursivePullSession(int recursionRadius, ModelContext context) {
        this.remainingDegreesOfSeparation = recursionRadius;
        this.partial = false;
        this.fieldNames = null;
        this.context = context;
    }

    public RecursivePullSession(int recursionRadius, ModelContext context, String... fieldNames) {
        this.remainingDegreesOfSeparation = recursionRadius;
        this.partial = true;
        this.fieldNames = fieldNames;
        this.context = context;
    }

    public void execute() {
        // each loop, we load models at +1 increasing radius (degrees of separation) from before.
        while (remainingDegreesOfSeparation >= 0) {
            remainingDegreesOfSeparation--;
            // switch queues
            Queue<Model> temp = workingPullQueue;
            workingPullQueue = pendingPullQueue;
            pendingPullQueue = temp;
            // go through working queue; each pull may add to the pending queue
            while (!workingPullQueue.isEmpty()) {
                if (partial) context.pullPartial(workingPullQueue.poll(), fieldNames);
                else context.pullAll(workingPullQueue.poll());
            }
        }
    }

    public void queue(Model model) {
        if (traversedIris.add(model.iri))
            pendingPullQueue.add(model);
    }
}
