package uk.ac.ceb.como.molhub.bean;

/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * The class provides a Java bean for electronic energy data. For example scf electronic energy, zero-point energy. It stores electronic energy value and electronic energy unit. 
 *
 */
public class ElectronicEnergy {
	
	private String electronicEnergyValue ;
	private String electronicEnergyUnit;
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param electronicEnergyValue the electronic energy value
	 * @param electronicEnergyUnit the electronic energy unit
	 * 
	 */
	public ElectronicEnergy (String electronicEnergyValue, String electronicEnergyUnit) {
		
		this.electronicEnergyValue=electronicEnergyValue;
		this.electronicEnergyUnit=electronicEnergyUnit;
	}


	public String getElectronicEnergyValue() {
		return electronicEnergyValue;
	}


	public void setElectronicEnergyValue(String electronicEnergyValue) {
		this.electronicEnergyValue = electronicEnergyValue;
	}


	public String getElectronicEnergyUnit() {
		return electronicEnergyUnit;
	}


	public void setElectronicEnergyUnit(String electronicEnergyUnit) {
		this.electronicEnergyUnit = electronicEnergyUnit;
	}
	
	
}
