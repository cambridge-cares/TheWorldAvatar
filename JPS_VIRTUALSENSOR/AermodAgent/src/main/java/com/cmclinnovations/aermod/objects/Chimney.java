package com.cmclinnovations.aermod.objects;

public class Chimney {
	// hardcoded values
	private double diameter = 1; // m
	private double height = 20; // m

    private double mixtureMolWeight; // kg/mol
	private double mixtureCp; // J/kg/K
	private double mixtureTemperature; // K
	private double mixtureMassFlux = 0.0192143028723584; // kg/s, constant in python script anyway, probably violates mass balance
	private double mixtureDensity; // kg/m3
	private double particleDensity; // kg/m3
	private double flowrateNOx; // kg/s
	private double flowrateSO2; //kg/s
	private double flowrateHC; //kg/s
	private double flowrateCO; //kg/s
	private double pm25; // pm 2.5
	private double pm10; // pm 10
    
    /**
     */
    public Chimney() {
    }

	public void setFlowrateNOx(double flowrateNOx) {
		this.flowrateNOx = flowrateNOx;
	}
	public void setFlowrateSO2(double flowrateSO2) {
		this.flowrateSO2 = flowrateSO2;
	}
	public void setFlowrateHC(double flowrateHC) {
		this.flowrateHC = flowrateHC;
	}
	public void setFlowrateCO(double flowrateCO) {
		this.flowrateCO = flowrateCO;
	}
	public void setMixtureTemperatureInKelvin(double mixtureTemperature) {
		this.mixtureTemperature = mixtureTemperature;
	}

    public double getMixtureTemperatureInKelvin() {
    	return this.mixtureTemperature;
    }
    public double getMixtureMassFlux() {
    	return this.mixtureMassFlux;
    }

	public void setMixtureDensityInKgm3(double mixtureDensity) {
		this.mixtureDensity = mixtureDensity;
	}
    public double getMixtureDensityInKgm3() {
    	return this.mixtureDensity;
    }
    public double getFlowrateNOxInKgs() {
    	return this.flowrateNOx;
    }
    public double getFlowrateCOInKgs() {
    	return this.flowrateCO;
    }
    public double getFlowrateSO2InKgs() {
    	return this.flowrateSO2;
    }
    public double getFlowrateHC() {
    	return this.flowrateHC;
    }
	public void setPM25(double pm25) {
		this.pm25 = pm25;
	}
	public double getPm25() {
		return this.pm25;
	}
	public void setPM10(double pm10) {
		this.pm10 = pm10;
	}
	public double getPm10() {
		return this.pm10;
	}
	public double getDiameter() {
    	return this.diameter;
    }
    public double getHeight() {
    	return this.height;
    }
	public double getMixtureMolWeight() {
		return this.mixtureMolWeight;
	}
	public double getMixtureCp() {
		return this.mixtureCp;
	}
	public void setParticleDensity(double particleDensity) {
		this.particleDensity = particleDensity;
	}
	public double getParticleDensity() {
		return particleDensity;
	}
}
