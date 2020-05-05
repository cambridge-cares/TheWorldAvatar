package uk.ac.ceb.como.ontokin.bean;

/**
 * Reports the status of a mechanism upload.
 * 
 * @author msff2
 *
 */
public class ChemkinUploadReport implements Cloneable{

	/** The name of the mechanism */
	private String mechanismName;
	
	/** The name of the mechanism file selected by user */
	private String mechanismFileName;
	
	/** The name of the thermodata file selected by user */
	private String thermoDataFileName;

	/** The name of the surface chemistry file selected by user */
	private String surfaceChemFileName;

	/** The name of the transport data file selected by user */
	private String transportDataFileName;
	
	/** Maintains info if the uploaded CHEMKIN files is valid */
	private boolean chemkinValidationReport;
	
	/** The validation report file. */
	private String chemkinValidationReportFile;

	/** Maintains info if the generated OntoKin OWL file is consistent */
	private boolean owlConsistencyReport;
	
	/**
	 * Default constructor of this class.
	 */
	public ChemkinUploadReport() {
		
	}
	
	/**
	 * Creates the report on a new mechanism upload.
	 * 
	 * @param mechanismName
	 * @param mechanismFileName
	 * @param thermoDataFileName
	 * @param surfaceChemFileName
	 * @param transportDataFileName
	 * @param chemkinValidationReport
	 * @param chemkinValidationReportFile
	 * @param owlConsistencyReport
	 */
	public ChemkinUploadReport(String mechanismName, String mechanismFileName, String thermoDataFileName, String surfaceChemFileName, String transportDataFileName, boolean chemkinValidationReport, String chemkinValidationReportFile, boolean owlConsistencyReport) {
		
		this.mechanismName=mechanismName;
		this.mechanismFileName=mechanismFileName;
		this.thermoDataFileName=thermoDataFileName;
		this.surfaceChemFileName=surfaceChemFileName;
		this.transportDataFileName=transportDataFileName;
		this.chemkinValidationReport=chemkinValidationReport;
		this.chemkinValidationReportFile=chemkinValidationReportFile;
		this.owlConsistencyReport=owlConsistencyReport;		
	}

	/**
	 * Gets the mechanism file name.
	 *
	 * @return the mechanism file name
	 */
	public String getMechanismFileName() {
		return mechanismFileName;
	}

	/**
	 * Sets the mechanism file name.
	 *
	 * @param mechanismFileName the new mechanism file name
	 */
	public void setMechanismFileName(String mechanismFileName) {
		this.mechanismFileName = mechanismFileName;
	}
	/**
	 * Returns the thermo data file name.
	 * 
	 * @return
	 */
	public String getThermoDataFileName() {
		return thermoDataFileName;
	}
	/**
	 * Sets the thermo data file name.
	 * 
	 * @param thermoDataFileName
	 */
	public void setThermoDataFileName(String thermoDataFileName) {
		this.thermoDataFileName = thermoDataFileName;
	}
	/**
	 * Returns the surface chemistry file name.
	 * 
	 * @return
	 */
	public String getSurfaceChemFileName() {
		return surfaceChemFileName;
	}
	/**
	 * Sets the surface chemistry file name.
	 * 
	 * @param surfaceChemFileName
	 */
	public void setSurfaceChemFileName(String surfaceChemFileName) {
		this.surfaceChemFileName = surfaceChemFileName;
	}
	/**
	 * Returns the transport data file name.
	 * 
	 * @return
	 */
	public String getTransportDataFileName() {
		return transportDataFileName;
	}
	/**
	 * Sets the transport data file name.
	 * 
	 * @param transportDataFileName
	 */
	public void setTransportDataFileName(String transportDataFileName) {
		this.transportDataFileName = transportDataFileName;
	}

	public boolean isChemkinValidationReport() {
		return chemkinValidationReport;
	}

	public void setChemkinValidationReport(boolean chemkinValidationReport) {
		this.chemkinValidationReport = chemkinValidationReport;
	}
	
	public String getChemkinValidationReportFile() {
		return chemkinValidationReportFile;
	}

	public void setChemkinValidationReportFile(String chemkinValidationReportFile) {
		this.chemkinValidationReportFile = chemkinValidationReportFile;
	}

	public boolean isOwlConsistencyReport() {
		return owlConsistencyReport;
	}

	public void setOwlConsistencyReport(boolean owlConsistencyReport) {
		this.owlConsistencyReport = owlConsistencyReport;
	}

	/**
	 * Returns the name of the current mechanism.
	 * 
	 * @return
	 */
	public String getMechanismName() {
		return mechanismName;
	}
	/**
	 * Sets the name of the current mechanism.
	 * 
	 * @param mechanismName
	 */
	public void setMechanismName(String mechanismName) {
		this.mechanismName = mechanismName;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }
	
    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return  "Mechanism name: " + getMechanismName() 
        + "|"+ "CHEMKIN mechanism file: " + getMechanismFileName() 
        + "|"+ "CHEMKIN thermo data file: " + getThermoDataFileName()
        + "|"+ "CHEMKIN surface chemistry file: " + getSurfaceChemFileName()
        + "|"+ "CHEMKIN transport data file: " + getTransportDataFileName()
        + " | "
                + "Validation of Compchem xml file: " + isChemkinValidationReport() 
                + " | "
                + "Consistency of Compchem owl file :  " + isOwlConsistencyReport();
    }
}