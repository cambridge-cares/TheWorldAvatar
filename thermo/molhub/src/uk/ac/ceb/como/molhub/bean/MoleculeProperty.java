package uk.ac.ceb.como.molhub.bean;

public class MoleculeProperty {

	private String uuid;
	private String moleculeName;
	private String levelOfTheory;
	private String basisSet;
	private int frequenciesSize;
	private String frequenciesValue;
	private String frequenciesUnit;
	private int spinMultiplicity;
	private double coordinateX;
	private double coordinateY;
	private double coordinateZ;
	private double atomicMass;
	private String atomicMassUnit;
	private String geometryType;
	private int rotationalConstantsSize;
	private String rotationalConstantsUnit;
	private String rotationalConstantsValue;
	private int rotationalSymmetryNumber;
	private String programName;
	private String programVersion;
	private String runDate;
	
	
	public MoleculeProperty() {
	}

	public MoleculeProperty(String uuid, String moleculeName, String levelOfTheory,
			String basisSet, int frequenciesSize, String frequenciesValue, String frequenciesUnit, int spinMultiplicity,
			double coordinateX, double coordinateY, double coordinateZ, double atomicMass, String atomicMassUnit,
			String geometryType, int rotationalConstantsSize, String rotationalConstantsUnit, String rotationalConstantsValue, int rotationalSymmetryNumber, String programName, String programVersion, String runDate) {

		this.uuid = uuid;
		this.moleculeName = moleculeName;
		this.levelOfTheory = levelOfTheory;
		this.basisSet = basisSet;
		this.frequenciesSize = frequenciesSize;
		this.frequenciesValue = frequenciesValue;
		this.frequenciesUnit = frequenciesUnit;
		this.spinMultiplicity = spinMultiplicity;
		this.coordinateX = coordinateX;
		this.coordinateY = coordinateY;
		this.coordinateZ = coordinateZ;
		this.atomicMass = atomicMass;
		this.atomicMassUnit = atomicMassUnit;
		this.geometryType = geometryType;
		this.rotationalConstantsSize = rotationalConstantsSize;
		this.rotationalConstantsUnit = rotationalConstantsUnit;
		this.rotationalConstantsValue=rotationalConstantsValue;
		this.rotationalSymmetryNumber=rotationalSymmetryNumber;
		this.programName=programName;
		this.programVersion=programVersion;
		this.runDate=runDate;

	}

	public MoleculeProperty(String uuid,String moleculeName, String basisSet, String levelOfTheory) {
		
		this.uuid=uuid;
		this.moleculeName=moleculeName;
		this.basisSet=basisSet;
		this.levelOfTheory=levelOfTheory;
		
	}
	
   public MoleculeProperty(String uuid,String moleculeName, String levelOfTheory) {
		
		this.uuid=uuid;
		this.moleculeName=moleculeName;		
		this.levelOfTheory=levelOfTheory;
		
	}

	
	public MoleculeProperty(String uuid, String moleculeName) {

		this.uuid = uuid;
		this.moleculeName = moleculeName;
	}

	public MoleculeProperty(String moleculeName) {

		this.moleculeName = moleculeName;

	}	

	public String getMoleculeName() {
		return moleculeName;
	}

	public void setMoleculeName(String moleculeName) {
		this.moleculeName = moleculeName;
	}
	
	public String getLevelOfTheory() {
		return levelOfTheory;
	}

	public void setLevelOfTheory(String levelOfTheory) {
		this.levelOfTheory = levelOfTheory;
	}

	public String getUuid() {
		return uuid;
	}

	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	public String getBasisSet() {
		return basisSet;
	}

	public void setBasisSet(String basisSet) {
		this.basisSet = basisSet;
	}

	public int getFrequenciesSize() {
		return frequenciesSize;
	}

	public void setFrequenciesSize(int frequenciesSize) {
		this.frequenciesSize = frequenciesSize;
	}

	public String getFrequenciesValue() {
		return frequenciesValue;
	}

	public void setFrequenciesValue(String frequenciesValue) {
		this.frequenciesValue = frequenciesValue;
	}

	public String getFrequenciesUnit() {
		return frequenciesUnit;
	}

	public void setFrequenciesUnit(String frequenciesUnit) {
		this.frequenciesUnit = frequenciesUnit;
	}

	public int getSpinMultiplicity() {
		return spinMultiplicity;
	}

	public void setSpinMultiplicity(int spinMultiplicity) {
		this.spinMultiplicity = spinMultiplicity;
	}

	public double getCoordinateX() {
		return coordinateX;
	}

	public void setCoordinateX(double coordinateX) {
		this.coordinateX = coordinateX;
	}

	public double getCoordinateY() {
		return coordinateY;
	}

	public void setCoordinateY(double coordinateY) {
		this.coordinateY = coordinateY;
	}

	public double getCoordinateZ() {
		return coordinateZ;
	}

	public void setCoordinateZ(double coordinateZ) {
		this.coordinateZ = coordinateZ;
	}

	public double getAtomicMass() {
		return atomicMass;
	}

	public void setAtomicMass(double atomicMass) {
		this.atomicMass = atomicMass;
	}

	public String getAtomicMassUnit() {
		return atomicMassUnit;
	}

	public void setAtomicMassUnit(String atomicMassUnit) {
		this.atomicMassUnit = atomicMassUnit;
	}

	public String getGeometryType() {
		return geometryType;
	}

	public void setGeometryType(String geometryType) {
		this.geometryType = geometryType;
	}

	public int getRotationalConstantsSize() {
		return rotationalConstantsSize;
	}

	public void setRotationalConstantsSize(int rotationalConstantsSize) {
		this.rotationalConstantsSize = rotationalConstantsSize;
	}

	public String getRotationalConstantsUnit() {
		return rotationalConstantsUnit;
	}

	public void setRotationalConstantsUnit(String rotationalConstantsUnit) {
		this.rotationalConstantsUnit = rotationalConstantsUnit;
	}

	public String getRotationalConstantsValue() {
		return rotationalConstantsValue;
	}

	public void setRotationalConstantsValue(String rotationalConstantsValue) {
		this.rotationalConstantsValue = rotationalConstantsValue;
	}

	public int getRotationalSymmetryNumber() {
		return rotationalSymmetryNumber;
	}

	public void setRotationalSymmetryNumber(int rotationalSymmetryNumber) {
		this.rotationalSymmetryNumber = rotationalSymmetryNumber;
	}

	public String getProgramName() {
		return programName;
	}

	public void setProgramName(String programName) {
		this.programName = programName;
	}

	public String getProgramVersion() {
		return programVersion;
	}

	public void setProgramVersion(String programVersion) {
		this.programVersion = programVersion;
	}

	public String getRunDate() {
		return runDate;
	}

	public void setRunDate(String runDate) {
		this.runDate = runDate;
	}

}
