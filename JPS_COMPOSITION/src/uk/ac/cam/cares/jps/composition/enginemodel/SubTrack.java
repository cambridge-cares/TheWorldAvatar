package uk.ac.cam.cares.jps.composition.enginemodel;

import java.net.URI;
import java.util.ArrayList;
import uk.ac.cam.cares.jps.composition.servicemodel.*;

public class SubTrack {

	public ArrayList<Service> servicesInvolved;
	private ArrayList<URI> messagePartsInvolved;
	public int score = 0;

	public SubTrack() {
		this.servicesInvolved = new ArrayList<Service>();
		this.messagePartsInvolved = new ArrayList<URI>();

	}

	public void CalculateScore() {
		this.score = 0 - messagePartsInvolved.size();
	}

	public ArrayList<Service> getServiceToBeRemoved() {
		return null;
	}

	public void addMessagePart(URI uri) {
		this.messagePartsInvolved.add(uri);
		this.CalculateScore();
	}

	public ArrayList<URI> getMessagePart() {
		return this.messagePartsInvolved;
	}

}
