package uk.ac.cam.cares.jps.composition.util;

import java.util.ArrayList;

import uk.ac.cam.cares.jps.composition.EngineModel.Edge;
import uk.ac.cam.cares.jps.composition.EngineModel.Graph;
import uk.ac.cam.cares.jps.composition.EngineModel.SubTrack;
import uk.ac.cam.cares.jps.composition.ServiceModel.MessagePart;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;

public class TraceParent {

	public ArrayList<SubTrack> subtracks;
	public SubTrack newSubTrack;

	public TraceParent() {
		this.subtracks = new ArrayList<SubTrack>();
	    this.newSubTrack = new SubTrack();
	}

	public void traceParentService(Graph graph, Edge aEdge, int counter) {

		if (aEdge.fromOutput[0] != -1) {
			Service previousService = graph.layers.get(aEdge.fromOutput[0]).getServices().get(aEdge.fromOutput[1]);
			Service thisService = graph.layers.get(aEdge.toInput[0]).getServices().get(aEdge.toInput[1]);
			for (MessagePart part : previousService.getAllInputs()) {
				if (part.inputEdges != null) {
					for (Edge inputEdge : part.inputEdges) {
						if (inputEdge.fromOutput != null) {
							newSubTrack.addMessagePart(part.uri);
							if (inputEdge.fromOutput[0] == -1) {
								subtracks.add(newSubTrack);
								newSubTrack = new SubTrack();
								newSubTrack.servicesInvolved.add(thisService);
								break;
							}
							else {
								traceParentService(graph, inputEdge, counter++);
							}

						}
					}
				}
			}
		}
	}

}
