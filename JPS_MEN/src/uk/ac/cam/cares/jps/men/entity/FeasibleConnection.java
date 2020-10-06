package uk.ac.cam.cares.jps.men.entity;

import java.util.List;

public class FeasibleConnection {

	private Source source = null;
	private Sink sink = null;
	private float distance = -1;
	
	/**
	 * @param connections
	 * @param source
	 * @param sink
	 * @return Returns the first connection that is found for given source and sink. Returns null if there is no such connection.
	 */
	public static FeasibleConnection findConnectionByNames(List<FeasibleConnection> connections, Source source, Sink sink) {
		for (FeasibleConnection connection : connections) {
			if (INamed.equalNames(connection.getSource(), source) 
					&& INamed.equalNames(connection.getSource().getProduct(), source.getProduct())
					&& INamed.equalNames(connection.getSink(), sink)
					&& INamed.equalNames(connection.getSink().getProduct(), sink.getProduct())) {
				return connection;
			}
		}		
		
		return null;
	}
	
	public FeasibleConnection(Source source, Sink sink) {
		this.source = source;
		this.sink = sink;
	}
	
	public Source getSource() {
		return source;
	}

	public Sink getSink() {
		return sink;
	}

	public float getDistance() {
		return distance;
	}
	
	public void setDistance(float distance) {
		this.distance = distance;
	}
	
	public String toString() {
		return "FeasibleConnection[ " + getSource() + ", " + getSink() + ", distance=" + getDistance() + "]";
	}
}
