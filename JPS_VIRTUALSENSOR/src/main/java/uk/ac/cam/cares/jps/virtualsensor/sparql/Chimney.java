package uk.ac.cam.cares.jps.virtualsensor.sparql;

import org.json.JSONArray;
import org.json.JSONObject;

public class Chimney {
	private double mixtureMolWeight; // kg/mol
	private double mixtureCp; // J/kg/K
	private double mixtureTemperature; // K
	private double mixtureMassFlux; // kg/s
	private double mixtureDensity; // kg/m3
	private Particle[] particles;
	private GasPollutant[] pollutants;
	/**
	 * input is the result from the speed load map, have not tested with SRM results
	 * All quantities should be in SI units
	 * @param result
	 */
    public Chimney(JSONObject result) {
    	JSONObject mixture = result.getJSONObject("mixture");
    	
    	// overall mixture properties
    	this.mixtureMolWeight = mixture.getJSONObject("molmass").getDouble("value");
    	this.mixtureCp = mixture.getJSONObject("cp").getDouble("value");
    	this.mixtureTemperature = mixture.getJSONObject("temperature").getDouble("value");
    	this.mixtureMassFlux = mixture.getJSONObject("massflux").getDouble("value");
    	this.mixtureDensity = mixture.getJSONObject("density").getDouble("value");
    	
    	// 1 'Particle' object for each size class
    	JSONArray particles_ja = result.getJSONArray("particle");
    	int numpar = particles_ja.length();
    	this.particles = new Particle[numpar];
    	double totalParticleFlowRate=0.0;
    	for (int i = 0; i < numpar; i++) {
    		this.particles[i] = new Particle();
    		this.particles[i].density = particles_ja.getJSONObject(i).getJSONObject("density").getDouble("value");
    		this.particles[i].flowrate = particles_ja.getJSONObject(i).getJSONObject("emission_rate").getDouble("value");
    		this.particles[i].diameter = particles_ja.getJSONObject(i).getJSONObject("diameter").getDouble("value")*1e-9; // convert from nm to m
    		totalParticleFlowRate += this.particles[i].getFlowrate();
    	}
    	// not sure if mass fraction is ever used
    	for (int i = 0; i < numpar; i++) {
    		this.particles[i].massFraction=this.particles[i].getFlowrate()/totalParticleFlowRate;
    	}
    	
    	// gas phase pollutants
    	JSONArray pollutants_ja = result.getJSONArray("pollutants");
    	int numpol = pollutants_ja.length();
    	this.pollutants = new GasPollutant[numpol];
    	for (int i = 0; i < numpol; i++) {
    		this.pollutants[i] = new GasPollutant();
    		this.pollutants[i].species = pollutants_ja.getJSONObject(i).getString("name");
    		this.pollutants[i].flowrate = pollutants_ja.getJSONObject(i).getDouble("value");
    	}
    }

    public double getMixtureMolWeight() {
    	return this.mixtureMolWeight;
    }
    public double getMixtureCp() {
    	return this.mixtureCp;
    }
    public double getMixtureTemperature() {
    	return this.mixtureTemperature;
    }
    public double getMixtureMassFlux() {
    	return this.mixtureMassFlux;
    }
    public double getMixtureDensity() {
    	return this.mixtureDensity;
    }
    public Particle getParticle(int i) {
    	return this.particles[i];
    }
    public GasPollutant getPollutant(int i) {
    	return this.pollutants[i];
    }
    public int getNumpar() {
    	return this.particles.length;
    }
    public int getNumpol() {
    	return this.pollutants.length;
    }
    
    public class Particle {
    	private double flowrate;
    	private double massFraction;
    	private double diameter;
    	private double density;
    	
    	public Particle() {}
    	
    	public double getFlowrate() {
    		return this.flowrate;
    	}
    	public double getMassFraction () {
    		return this.massFraction;
    	}
    	public double getDiameter() {
    		return this.diameter;
    	}
    	public double getDensity() {
    		return this.density;
    	}
    }

    public class GasPollutant {
    	private String species;
    	private double flowrate;
    	
    	public GasPollutant() {}
    	
    	public String getSpecies() {
    		return this.species;
    	}
    	public double getFlowrate() {
    		return this.flowrate;
    	}
    }
}


