package uk.ac.cam.cares.jps.powsys.nuclear;

public class NuclearGenType {
	private String nucleargentype = "";
	private double capacity = 0;
	private String id = "";
	
	public NuclearGenType(String NuclearGenType) {
		this.nucleargentype = NuclearGenType;
	}
	
	public String getnucleargen() {
		return nucleargentype;
	}
	
	public double getcapacity() {
		return capacity;
	}
	public void setcapacity(double capacity) {
		this.capacity = capacity;
	}
	
	public String getid() {
		return id;
	}
	public void setid(String id) {
		this.id = id;
	}

	@Override
	public boolean equals(Object other) {
		if (this == other) {
			return true;
		}

		if ((other == null) || !(other instanceof NuclearGenType)) {
			return false;
		}

		NuclearGenType otherGenType = (NuclearGenType) other;
		return this.nucleargentype.equals(otherGenType.nucleargentype) &&
				this.capacity == otherGenType.capacity &&
				this.id.equals(otherGenType.id);
	}
}
