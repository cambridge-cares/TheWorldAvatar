package com.cmclinnovations.aermod.objects;

public class PointSource {
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

    public void setFlowrateCO2InTonsPerYear(double flowrateCO2InTonsPerYear) {
        flowrateCO2 = flowrateCO2InTonsPerYear * 1000 * 1000 / (365 * 24 * 60 * 60);
    }

    public double getFlowrateCO2InGramsPerSecond() {
        return flowrateCO2;
    }

    public void setFlowrateNOxInKgPerS(double flowrateNOxInKgPerS) {
        this.flowrateNOx = flowrateNOxInKgPerS * 1000;
    }

    public void setFlowrateSO2InKgPerS(double flowrateSO2InKgPerS) {
        this.flowrateSO2 = flowrateSO2InKgPerS * 1000;
    }

    public void setFlowrateHCInKgPerS(double flowrateHCInKgPerS) {
        this.flowrateHC = flowrateHCInKgPerS * 1000;
    }

    public void setFlowrateCOInKgPerS(double flowrateCOInKgPerS) {
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
