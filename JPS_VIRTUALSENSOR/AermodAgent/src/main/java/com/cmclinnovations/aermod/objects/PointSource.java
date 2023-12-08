package com.cmclinnovations.aermod.objects;

import java.util.EnumMap;
import java.util.Map;

import org.locationtech.jts.geom.Point;

import com.cmclinnovations.aermod.objects.Pollutant.PollutantType;

public class PointSource {
    private Point location;
    private double diameter; // m
    private double height; // m

    private double mixtureMolWeight; // kg/mol
    private double mixtureCp; // J/kg/K
    private double mixtureTemperature; // K
    private double mixtureDensity; // kg/m3
    private double particleDensity; // kg/m3

    private Map<PollutantType, Double> flowrateInGramsPerS;

    private String iri;
    private double baseElevation = 0;

    public PointSource(String iri) {
        this.iri = iri;
        flowrateInGramsPerS = new EnumMap<>(PollutantType.class);
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

    public void setElevation(double elevation) {
        this.baseElevation = elevation;
    }

    public double getElevation() {
        return baseElevation;
    }

    public void setFlowrateInKgPerS(PollutantType pollutant, double flowRate) {
        flowrateInGramsPerS.put(pollutant, flowRate * 1000);
    }

    public double getFlowrateInGramsPerS(PollutantType pollutant) {
        return flowrateInGramsPerS.get(pollutant);
    }

    public boolean hasPollutant(PollutantType pollutant) {
        return flowrateInGramsPerS.containsKey(pollutant);
    }

    public void setMixtureTemperatureInKelvin(double mixtureTemperature) {
        this.mixtureTemperature = mixtureTemperature;
    }

    public double getMixtureTemperatureInKelvin() {
        return this.mixtureTemperature;
    }

    public double getMixtureMassFlux() {
        return flowrateInGramsPerS.values().stream().mapToDouble(d -> d).sum() / 1000;
    }

    public void setMixtureDensityInKgm3(double mixtureDensity) {
        this.mixtureDensity = mixtureDensity;
    }

    public double getMixtureDensityInKgm3() {
        return this.mixtureDensity;
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
