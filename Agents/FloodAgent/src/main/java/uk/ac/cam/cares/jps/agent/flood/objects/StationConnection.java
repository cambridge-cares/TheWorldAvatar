package uk.ac.cam.cares.jps.agent.flood.objects;

public class StationConnection {
    private Station upstream;
    private Station downstream;

    public StationConnection(Station upstream, Station downstream) {
        this.upstream = upstream;
        this.downstream = downstream;
        upstream.setDownstream(downstream);
        downstream.setUpstream(upstream);
    }

    public Station getUpstream() {
        return this.upstream;
    }

    public Station getDownstream() {
        return this.downstream;
    }
}
