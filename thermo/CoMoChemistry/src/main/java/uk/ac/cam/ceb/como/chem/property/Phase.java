package uk.ac.cam.ceb.como.chem.property;

public enum Phase {

    Aqueous("aqueous"),
    Gaseous("gaseous"),
    Liquid("liquid"),
    Solid("solid"),
    Crystalline("crystalline"),
    Unknown("unknown");
    
	private final String phase;

    Phase(String phase) {
        this.phase = phase;
    }

    public String getPhaseString() {
        return phase;
    }

    @Override
    public String toString() {
        return "Phase{" + phase + '}';
    }

    public static Phase parse(String phaseString) {
        phaseString = phaseString.trim();
        if (phaseString.equalsIgnoreCase("a") || phaseString.equalsIgnoreCase("aq") || phaseString.equalsIgnoreCase("aqueous")) {
            return Aqueous;
        } else if (phaseString.equalsIgnoreCase("g") || phaseString.equalsIgnoreCase("gas") || phaseString.equalsIgnoreCase("gaseous")) {
            return Gaseous;
        } else if (phaseString.equalsIgnoreCase("l") || phaseString.equalsIgnoreCase("liquid")) {
            return Liquid;
        } else if (phaseString.equalsIgnoreCase("s") || phaseString.equalsIgnoreCase("solid")) {
            return Solid;
        } else if (phaseString.equalsIgnoreCase("c") || phaseString.equalsIgnoreCase("cr") || phaseString.equalsIgnoreCase("crystalline")) {
            return Crystalline;
        } else {
            return Unknown;
        }
    }
}
