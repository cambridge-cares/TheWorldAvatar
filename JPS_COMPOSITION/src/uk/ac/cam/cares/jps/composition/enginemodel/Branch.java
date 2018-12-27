package uk.ac.cam.cares.jps.composition.EngineModel;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;

import uk.ac.cam.cares.jps.composition.ServiceModel.Service;

public class Branch { 
	public ArrayList<SubTrack> subtracks;
	public HashSet<URI> messagePartInvolved;
	public SubTrack selectedSubtrack;
	public HashSet<Service> servicesToBeDeleted;

	public Branch() {
		this.subtracks = new ArrayList<SubTrack>();
		this.messagePartInvolved = new HashSet<URI>();
		this.servicesToBeDeleted = new HashSet<Service>();
	}

	public void collectMessagePartInvolved() {
		for (int i = 0; i < this.subtracks.size(); i++) {
			this.messagePartInvolved.addAll(this.subtracks.get(i).getMessagePart());
		}
	}

	public void removeRedundantSubtracks(Graph graph) {
		double max = -1000;
		for (int i = 0; i < this.subtracks.size(); i++) {
			SubTrack subtrack = subtracks.get(i);
			subtrack.CalculateScore(graph.scoreMap);
			if (subtrack.score > max) {
				max = subtrack.score;
				this.selectedSubtrack = subtrack;
			}
		}
		this.messagePartInvolved.removeAll(this.selectedSubtrack.getMessagePart());
		for(URI input: this.messagePartInvolved) {
			this.servicesToBeDeleted.add(graph.findServiceByInput(input));
		}

	}

	public boolean checkSubSet(Branch anotherBranch) {
		this.collectMessagePartInvolved();
		anotherBranch.collectMessagePartInvolved();
		if (this.messagePartInvolved.containsAll(anotherBranch.messagePartInvolved)) {
			return true;
		} else {
			return false;
		}

	}

}
