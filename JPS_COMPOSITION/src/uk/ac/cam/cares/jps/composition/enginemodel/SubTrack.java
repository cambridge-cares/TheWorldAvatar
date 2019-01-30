package uk.ac.cam.cares.jps.composition.enginemodel;

import java.net.URI;
import java.util.ArrayList;
import java.util.Map;

import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class SubTrack {

	public ArrayList<Service> servicesInvolved;
	private ArrayList<URI> messagePartsInvolved;
	public double score = 0;

	public SubTrack() {
		this.servicesInvolved = new ArrayList<Service>();
		this.messagePartsInvolved = new ArrayList<URI>();
	}

	public void CalculateScore(Map<String, Long[]> scoreMap) {
		// The map from the graph contains a map of score...
		
		double totalScore = 0;
		for(Service service : this.servicesInvolved) {
			// The key of the map is the iri of a agent. 
			String iri = service.getUri().toASCIIString();
			
			System.out.println("IRI" + iri);
			System.out.println(scoreMap.keySet());
			
			if(scoreMap.containsKey(iri)) {
				Long[] scoreArray = scoreMap.get(iri);		
				double score = scoreArray[0] / scoreArray[1];
				totalScore = totalScore + score;
			}
		}
		if(totalScore == 0) {
			this.score = 5;
		}
		else {
			this.score = totalScore;
		}
		
	}

	public ArrayList<Service> getServiceToBeRemoved() {
		return null;
	}

	public void addMessagePart(URI uri) {
		this.messagePartsInvolved.add(uri);
		//this.CalculateScore();
	}

	public ArrayList<URI> getMessagePart() {
		return this.messagePartsInvolved;
	}

}
