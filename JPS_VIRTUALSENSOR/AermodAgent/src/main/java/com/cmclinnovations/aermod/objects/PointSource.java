package com.cmclinnovations.aermod.objects;

import org.locationtech.jts.geom.Point;

public class PointSource {
    private Point location;
    private double diameter; // m
    private double height; // m

    private double mixtureMolWeight; // kg/mol
    private double mixtureCp; // J/kg/K
    private double mixtureTemperature; // K
    private double mixtureMassFlux = 0.0192143028723584; // kg/s, constant in python script anyway, probably violates
                                                         // mass balance
    private double mixtureDensity; // kg/m3
    private double particleDensity; // kg/m3
    private double flowrateNOx; // g/s
    private double flowrateSO2; // g/s
    private double flowrateHC; // g/s
    private double flowrateCO; // g/s
    private double flowrateCO2;
    private double pm25; // pm 2.5
    private double pm10; // pm 10

    private String iri;

    public PointSource(String iri) {
        this.iri = iri;
    }

    public String getIri() {
        return iri;
    }

    public void setLocation(Point location) {
        this.location = location;
    }

    public Point getLocation() {
        return this.location;
    }

    public void setFlowRateCO2InKgPerS(double flowrateCO2InKgPerS) {
        flowrateCO2 = flowrateCO2InKgPerS * 1000;
    }

    public double getFlowrateCO2InGramsPerSecond() {
        return flowrateCO2;
    }

    public void setFlowRateNOxInKgPerS(double flowrateNOxInKgPerS) {
        this.flowrateNOx = flowrateNOxInKgPerS * 1000;
    }

    public void setFlowRateSO2InKgPerS(double flowrateSO2InKgPerS) {
        this.flowrateSO2 = flowrateSO2InKgPerS * 1000;
    }

    public void setFlowRateHCInKgPerS(double flowrateHCInKgPerS) {
        this.flowrateHC = flowrateHCInKgPerS * 1000;
    }

    public void setFlowRateCOInKgPerS(double flowrateCOInKgPerS) {
        this.flowrateCO = flowrateCOInKgPerS * 1000;
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

    public double getFlowrateNOxInGramsPerS() {
        return this.flowrateNOx;
    }

    public double getFlowrateCOInGramsPerS() {
        return this.flowrateCO;
    }

    public double getFlowrateSO2InGramsPerS() {
        return this.flowrateSO2;
    }

    public double getFlowrateHCInGramsPerS() {
        return this.flowrateHC;
    }

    public void setFlowRatePM25InKgPerS(double flowratepm25InKgPerS) {
        this.pm25 = flowratepm25InKgPerS * 1000;
    }

    public double getFlowRatePm25InGramsPerS() {
        return this.pm25;
    }

    public void setFlowRatePM10InKgPerS(double flowratepm10InKgPerS) {
        this.pm10 = flowratepm10InKgPerS * 1000;
    }

    public double getFlowRatePm10InGramsPerS() {
        return this.pm10;
    }

    public double getDiameter() {
        return this.diameter;
    }

    public void setDiameter(double diameter) {
        this.diameter = diameter;
    }

    public double getHeight() {
        return this.height;
    }

    public void setHeight(double height) {
        this.height = height;
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
