package uk.ac.cam.cares.jps.agent.gfaagent;

import java.util.HashMap;
import java.util.Map;

public class Building {
    String iri;
    Map<String, Double> usageToShareMap;
    double cost;
    double gfa;

    public Building(String iri) {
        this.iri = iri;
        usageToShareMap = new HashMap<>();
    }

    public String getIri() {
        return iri;
    }

    public void addUsageShare(String usage, double usageShare) {
        if (usageToShareMap.containsKey(usage)) {
            throw new RuntimeException("Duplicate usage?");
        }
        usageToShareMap.put(usage, usageShare);
    }

    public Map<String, Double> getUsageShare() {
        return usageToShareMap;
    }

    public void setCost(double cost) {
        this.cost = cost;
    }

    public double getCost() {
        return cost;
    }

    public void setGFA(double gfa) {
        this.gfa = gfa;
    }

    public double getGFA() {
        return gfa;
    }
}
