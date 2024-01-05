package uk.ac.cam.cares.jps.agent.heat.objects;

public class Factory {

    /*
     * Units of production volume is specified as tons per year and converted
     * to kg/s in this class.
     * Units of specific energy is Megajoules per kilogram.
     * Units of heatEmissions is Megawatt
     */
    public double productionVolume = 0.0;
    public double specificEnergy = 0.0;
    public double thermalEfficiency = 0.52;
    public double heatEmission = 0.0;
    public String iri = null;
    private static final Integer numberOfSecondsPerYear = 365 * 24 * 60 * 60;

    FactoryType facility = null;

    public Factory(String iri, FactoryType facilityType) {
        this.iri = iri;
        this.facility = facilityType;
    }

    public Factory(String iri, FactoryType facilityType,
            double productionVolume, double specificEnergy, double thermalEfficiency) {
        this.iri = iri;
        this.facility = facilityType;
        this.productionVolume = productionVolume * 1000 / numberOfSecondsPerYear;
        this.specificEnergy = specificEnergy;
        this.thermalEfficiency = thermalEfficiency;
    }

    public void calculateHeat() {

        switch (facility) {
            case ChemicalPlant:
                heatEmission = specificEnergy < 0.0 ? -1.0 * productionVolume * specificEnergy
                        : productionVolume * specificEnergy * thermalEfficiency;
                break;
            case BeveragePlant:
                heatEmission = 0.0;
                break;
            case FoodPlant:
                heatEmission = 0.0;
                break;
            case PharmaceuticalPlant:
                heatEmission = 0.0;
                break;
            case PrecisionEngineeringPlant:
                heatEmission = 0.0;
                break;
            case DataCenter:
                heatEmission = 0.0;
                break;
            case PrintingPlant:
                heatEmission = 0.0;
                break;
        }
    }

}
