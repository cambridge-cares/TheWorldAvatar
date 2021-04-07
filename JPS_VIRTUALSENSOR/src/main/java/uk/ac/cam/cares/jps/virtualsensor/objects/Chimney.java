package uk.ac.cam.cares.jps.virtualsensor.objects;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.virtualsensor.sparql.ShipSparql;

public class Chimney {
	private double mixtureMolWeight; // kg/mol
	private double mixtureCp; // J/kg/K
	private double mixtureTemperature; // K
	private double mixtureMassFlux; // kg/s
	private double mixtureDensity; // kg/m3
	private double totalParticleFlowrate; // kg/s
	private double diameter; // m
	private double height; // m
	private Particle[] particles;
	private GasPollutant[] pollutants;
	private double flowrateNOx;
	private double flowrateSO2;
	private double flowrateHC;
	private double flowrateCO;
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
    	double totalParticleFlowrate=0.0;
    	for (int i = 0; i < numpar; i++) {
    		this.particles[i] = new Particle();
    		this.particles[i].setDensity(particles_ja.getJSONObject(i).getJSONObject("density").getDouble("value"));
    		this.particles[i].setFlowrate(particles_ja.getJSONObject(i).getJSONObject("emission_rate").getDouble("value"));
    		this.particles[i].setDiameter(particles_ja.getJSONObject(i).getJSONObject("diameter").getDouble("value")*1e-9); // convert from nm to m
    		totalParticleFlowrate += this.particles[i].getFlowrate();
    	}
    	
    	// not sure if mass fraction is ever used
    	for (int i = 0; i < numpar; i++) {
    		this.particles[i].setMassFraction(this.particles[i].getFlowrate()/totalParticleFlowrate);
    	}
    	this.totalParticleFlowrate = totalParticleFlowrate;
    	
    	// gas phase pollutants
    	JSONArray pollutants_ja = result.getJSONArray("pollutants");
    	int numpol = pollutants_ja.length()+1; // last element is for NOx
    	this.pollutants = new GasPollutant[numpol];
    	
    	// initialise manually for NOx
    	this.pollutants[numpol-1] = new GasPollutant();
    	this.pollutants[numpol-1].setSpecies("NOx");
    	double NOxflowrate = 0.0;
    	
    	for (int i = 0; i < numpol-1; i++) {
    		this.pollutants[i] = new GasPollutant();
    		this.pollutants[i].setSpecies(pollutants_ja.getJSONObject(i).getString("name"));
    		this.pollutants[i].setFlowrate(pollutants_ja.getJSONObject(i).getDouble("value"));
    		
    		if (this.pollutants[i].getSpecies().contains("NO") || this.pollutants[i].getSpecies().contains("NO2")) {
    			NOxflowrate += this.pollutants[i].getFlowrate();
    		}
    	}
    	
    	this.pollutants[numpol-1].setFlowrate(NOxflowrate);
    }
    
    // to be used in Episode agent
    public Chimney(String ship_iri_string) {
    	JSONArray ChimneyProperties = ShipSparql.QueryChimneyProperties(ship_iri_string);
    	this.diameter = ChimneyProperties.getJSONObject(0).getDouble("diameter");
    	this.mixtureMassFlux = ChimneyProperties.getJSONObject(0).getDouble("overallFlowrate");
    	this.mixtureDensity = ChimneyProperties.getJSONObject(0).getDouble("density");
    	this.height = ChimneyProperties.getJSONObject(0).getDouble("height");
    	this.mixtureTemperature = ChimneyProperties.getJSONObject(0).getDouble("temp");
    	this.totalParticleFlowrate = ChimneyProperties.getJSONObject(0).getDouble("particleFlowrate");
    	
    	//gas phase emissions
    	this.flowrateNOx = ShipSparql.QueryMixtureFlowrate(ship_iri_string, "NOx");
    	this.flowrateCO = ShipSparql.QueryMixtureFlowrate(ship_iri_string, "CO");
    	this.flowrateSO2 = ShipSparql.QueryMixtureFlowrate(ship_iri_string, "SO2");
    	this.flowrateHC = ShipSparql.QueryMixtureFlowrate(ship_iri_string, "HC");
    	
    	// particle phase
    	String[] particleIRI = ShipSparql.QueryParticleIRI(ship_iri_string);
    	this.particles = new Particle[particleIRI.length];
    	for (int i=0; i < particleIRI.length; i++) {
    		// mass fraction, density, and diameter are queried from the triplestore, not flowrate.
    		this.particles[i] = ShipSparql.QueryParticle(particleIRI[i]);
    		this.particles[i].setFlowrate(this.particles[i].getMassFraction()*this.totalParticleFlowrate);
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
    public double getTotalParticleFlowrate() {
    	return this.totalParticleFlowrate;
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
    public double getDiameter() {
    	return this.diameter;
    }
    public double getHeight() {
    	return this.height;
    }
    public double getFlowrateNOx() {
    	return this.flowrateNOx;
    }
    public double getFlowrateCO() {
    	return this.flowrateCO;
    }
    public double getFlowrateSO2() {
    	return this.flowrateSO2;
    }
    public double getFlowrateHC() {
    	return this.flowrateHC;
    }
}


