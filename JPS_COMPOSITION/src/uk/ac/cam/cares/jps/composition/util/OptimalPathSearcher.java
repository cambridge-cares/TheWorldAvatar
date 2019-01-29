package uk.ac.cam.cares.jps.composition.util;

import java.util.ArrayList;
import java.util.HashSet;

import uk.ac.cam.cares.jps.composition.enginemodel.Branch;
import uk.ac.cam.cares.jps.composition.enginemodel.Edge;
import uk.ac.cam.cares.jps.composition.enginemodel.Graph;
import uk.ac.cam.cares.jps.composition.enginemodel.SubTrack;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class OptimalPathSearcher {

	private Graph theGraph;

	public OptimalPathSearcher(Graph theGraph) {
		this.setTheGraph(theGraph);// Waiting for the next iteration of upgrading...
	}

	public void searchForTheOptimalPath() {

	}

	public static ArrayList<Service> getAllServicesToBeDeleted(Graph graph) {
		ArrayList<Service> servicesToBeDeleted = new ArrayList<Service>();
		ArrayList<Branch> branches = OptimalPathSearcher.getAllBranches(graph);
		for (int i = 0; i < branches.size(); i++) {
			for (int j = 0; j < branches.size(); j++) {
				Branch branch = branches.get(i);
				Branch branch2 = branches.get(j);
				if (branch != branch2) {
					if (branch.checkSubSet(branch2)) {
						branches.remove(branch2);
					}
				}
			}
		}

		for (Branch branch : branches) {
			branch.removeRedundantSubtracks(graph);
			for (Service service : branch.servicesToBeDeleted) {
				servicesToBeDeleted.add(service);
			}
		}

		return servicesToBeDeleted;

	}

	public static ArrayList<Branch> getAllBranches(Graph graph) {
		ArrayList<Branch> allBranches = new ArrayList<Branch>();
		for (int i = graph.layers.size() - 1; i >= 0; i--) {
			for (Service service : graph.layers.get(i).getServices()) {
				Branch newBranch = new Branch();
				for (MessagePart part : service.getAllInputs()) {
					boolean containsOption = false;
					if (part.inputEdges.size() > 1) {
						for (Edge aEdge : new HashSet<Edge>(part.inputEdges)) {
							if (aEdge.fromOutput != null) {
								if (aEdge.fromOutput[0] != -1) {
									TraceParent tracer = new TraceParent();
									tracer.traceParentService(graph, aEdge, 0); // find all independent
									for (SubTrack sub : tracer.subtracks) {
										newBranch.subtracks.add(sub);
									}
									containsOption = true;
								}
							}
						}
					}
					if (containsOption) {
						allBranches.add(newBranch);
						newBranch = new Branch();
					}
				}
			}
		}
		return allBranches;
	}

	public Graph getTheGraph() {
		return theGraph;
	}

	public void setTheGraph(Graph theGraph) {
		this.theGraph = theGraph;
	}
}