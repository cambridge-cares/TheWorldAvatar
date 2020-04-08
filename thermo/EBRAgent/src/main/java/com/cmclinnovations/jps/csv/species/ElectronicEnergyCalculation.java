package com.cmclinnovations.jps.csv.species;
/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 *
 */
public class ElectronicEnergyCalculation {

	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param scfEnergy the scf energy
	 * @param zeroPointEnergy the zero point energy
	 * @return the electronic energy that is sum of scf energy and zero point energy
	 */
	public static String getElectronicEnergy(String scfEnergy, String zeroPointEnergy) {
		
		double scfEnergyd = Double.valueOf(scfEnergy);
		double zeroEnergyd = Double.valueOf(zeroPointEnergy);
		
		double energySum = scfEnergyd + zeroEnergyd;
		
		String electronicEnergy= String.valueOf(energySum);
		
		return electronicEnergy;
	}
}
