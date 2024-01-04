package uk.ac.cam.cares.jps.agent.heat.objects;

public class ChemicalPlant {

    public double productionVolume = 0.0;
    public double specificEnergy = 0.0;
    public double thermalEfficiency = 0.52;
    public double heatEmission = 0.0;
    public String iri = null;

    public ChemicalPlant(double productionVolume, double specificEnergy, String iri) {
        this.productionVolume = productionVolume;
        this.specificEnergy = specificEnergy;
        this.iri = iri;
    }

    void calculateHeat() {
        if (specificEnergy < 0.0)
            heatEmission = -1.0 * productionVolume * specificEnergy;
        else
            heatEmission = productionVolume * specificEnergy;
    }

}
