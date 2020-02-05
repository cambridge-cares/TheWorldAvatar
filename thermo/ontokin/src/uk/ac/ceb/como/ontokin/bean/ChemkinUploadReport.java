package uk.ac.ceb.como.ontokin.bean;

// TODO: Auto-generated Javadoc
/**
 * The Class GaussianUploadReport.
 */
public class ChemkinUploadReport implements Cloneable{

	/** The uuid. */
	private String mechanismName;
	
	/** The mechanism file name. */
	private String mechanismFileName;
	
	/** The thermo data file name. */
	private String thermoDataFileName;

	/** The surface chemistry file name. */
	private String surfaceChemFileName;

	/** The transport data file name. */
	private String transportDataFileName;
	
	/** The validation compchem file. */
	private boolean chemkinValidationReport;
	
	/** The consistency compchem ontology file. */
	private boolean owlConsistencyReport;
	
	/**
	 * Instantiates a new gaussian upload report.
	 */
	public ChemkinUploadReport() {
		
	}
	
	/**
	 * Instantiates a new gaussian upload report.
	 *
	 * @param uuid the uuid
	 * @param gaussianFileName the gaussian file name
	 * @param validationCompchemFile the validation of compchem file
	 * @param consistencyCompchemOntologyFile the consistency of compchem ontology file
	 */
	public ChemkinUploadReport(String mechanismName, String mechanismFileName, String thermoDataFileName, String surfaceChemFileName, String transportDataFileName, boolean chemkinValidationReport, boolean owlConsistencyReport) {
		
		this.mechanismName=mechanismName;
		this.mechanismFileName=mechanismFileName;
		this.thermoDataFileName=thermoDataFileName;
		this.surfaceChemFileName=surfaceChemFileName;
		this.transportDataFileName=transportDataFileName;
		this.chemkinValidationReport=chemkinValidationReport;
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