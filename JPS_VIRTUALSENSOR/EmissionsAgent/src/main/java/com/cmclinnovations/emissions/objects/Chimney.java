package com.cmclinnovations.emissions.objects;

import org.json.JSONArray;
import org.json.JSONObject;

public class Chimney {
    // hardcoded values
    private double diameter = 1; // m
    private double height = 20; // m

    private static final String VALUE_STRING = "value";
    private double mixtureMolWeight; // kg/mol
    private double mixtureCp; // J/kg/K
    private double mixtureTemperature; // K
    private double mixtureMassFlux = 0.0192143028723584; // kg/s, constant in python script anyway, probably violates
                                                         // mass balance
    private double mixtureDensity; // kg/m3

    private double particleDensity; // kg/m3

    private double flowrateNOx;
    private double flowrateSO2;
    private double flowrateHC;
    private double flowrateCO;
    private double pm25; // pm 2.5
    private double pm10; // pm 10

    /**
     * input is result from speed load map python script
     * 
     * @param result
     */
    public Chimney(JSONObject result) {
        JSONObject mixture = result.getJSONObject("mixture");

        // overall mixture properties
        this.mixtureMolWeight = mixture.getJSONObject("molmass").getDouble(VALUE_STRING);
        this.mixtureCp = mixture.getJSONObject("cp").getDouble(VALUE_STRING);
        this.mixtureTemperature = mixture.getJSONObject("temperature").getDouble(VALUE_STRING);
        this.mixtureDensity = mixture.getJSONObject("density").getDouble(VALUE_STRING);

        // 1 'Particle' object for each size class
        JSONArray particlesArray = result.getJSONArray("particle");
        int numpar = particlesArray.length();
        Particle[] particles = new Particle[numpar];
        for (int i = 0; i < numpar; i++) {
            particles[i] = new Particle();
            particles[i].setDensity(particlesArray.getJSONObject(i).getJSONObject("density").getDouble(VALUE_STRING));
            particles[i].setFlowrate(
                    particlesArray.getJSONObject(i).getJSONObject("emission_rate").getDouble(VALUE_STRING));
            particles[i].setDiameter(
                    particlesArray.getJSONObject(i).getJSONObject("diameter").getDouble(VALUE_STRING) * 1e-9); // convert
                                                                                                               // from
                                                                                                               // nm to
                                                                                                               // m
            if (particles[i].getDiameter() <= 0.0000025) {
                this.pm25 += particles[i].getFlowrate();
            }
            if (particles[i].getDiameter() <= 0.00001) {
                this.pm10 += particles[i].getFlowrate();
            }
        }

        // they're all the same
        this.particleDensity = particles[0].getDensity();

        // gas phase pollutants
        JSONArray pollutantsArray = result.getJSONArray("pollutants");
        int numpol = pollutantsArray.length() + 1; // last element is for NOx
        GasPollutant[] pollutants = new GasPollutant[numpol];

        // initialise manually for NOx
        pollutants[numpol - 1] = new GasPollutant();
        pollutants[numpol - 1].setSpecies("NOx");

        for (int i = 0; i < numpol - 1; i++) {
            pollutants[i] = new GasPollutant();
            pollutants[i].setSpecies(pollutantsArray.getJSONObject(i).getString("name"));
            pollutants[i].setFlowrate(pollutantsArray.getJSONObject(i).getDouble(VALUE_STRING));

            // NOx is sum of NO2 and NO, the others are obtained directly from python
            // results
            if (pollutants[i].getSpecies().contentEquals("NO") || pollutants[i].getSpecies().contentEquals("NO2")) {
                this.flowrateNOx += pollutants[i].getFlowrate();
            } else if (pollutants[i].getSpecies().contentEquals("HC")) {
                this.flowrateHC = pollutants[i].getFlowrate();
            } else if (pollutants[i].getSpecies().contentEquals("CO")) {
                this.flowrateCO = pollutants[i].getFlowrate();
            } else if (pollutants[i].getSpecies().contentEquals("SO2")) {
                this.flowrateSO2 = pollutants[i].getFlowrate();
            }
        }
        pollutants[numpol - 1].setFlowrate(flowrateNOx);
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

    public double getPm25() {
        return this.pm25;
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

    public double getParticleDensity() {
        return this.particleDensity;
    }
}
