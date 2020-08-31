package uk.ac.ceb.como.molhub.bean;

// TODO: Auto-generated Javadoc
/**
 * The Class GaussianUploadReport.
 */
public class GaussianUploadReport implements Cloneable{

	/** The uuid. */
	private String uuid;
	
	/** The gaussian file name. */
	private String gaussianFileName;
	
	private String owlFileName ;
	
	/** The validation compchem file. */
	private boolean validationCompchemFile;
	
	/** The consistency compchem ontology file. */
	private boolean consistencyCompchemOntologyFile;
	
	/**
	 * Instantiates a new gaussian upload report.
	 */
	public GaussianUploadReport() {
		
	}
	
	/**
	 * Instantiates a new gaussian upload report.
	 *
	 * @param uuid the uuid
	 * @param gaussianFileName the gaussian file name
	 * @param validationCompchemFile the validation of compchem file
	 * @param consistencyCompchemOntologyFile the consistency of compchem ontology file
	 */
	public GaussianUploadReport(String uuid, String gaussianFileName, boolean validationCompchemFile, boolean consistencyCompchemOntologyFile) {
		
		this.uuid=uuid;
		this.gaussianFileName=gaussianFileName;
		this.validationCompchemFile=validationCompchemFile;
		this.consistencyCompchemOntologyFile=consistencyCompchemOntologyFile;		
	}


	public GaussianUploadReport(String uuid, String gaussianFileName, String owlFileName, boolean consistencyCompchemOntologyFile) {
		
		this.uuid=uuid;
		this.gaussianFileName=gaussianFileName;
		this.owlFileName=owlFileName;
		this.consistencyCompchemOntologyFile=consistencyCompchemOntologyFile;		
	}

	/**
	 * Gets the gaussian file name.
	 *
	 * @return the gaussian file name
	 */
	public String getGaussianFileName() {
		return gaussianFileName;
	}

	/**
	 * Sets the gaussian file name.
	 *
	 * @param gaussianFileName the new gaussian file name
	 */
	public void setGaussianFileName(String gaussianFileName) {
		this.gaussianFileName = gaussianFileName;
	}

	/**
	 * Checks if is validation compchem file.
	 *
	 * @return true, if is validation compchem file
	 */
	public boolean isValidationCompchemFile() {
		return validationCompchemFile;
	}

	/**
	 * Sets the validation compchem file.
	 *
	 * @param validationCompchemFile the new validation compchem file
	 */
	public void setValidationCompchemFile(boolean validationCompchemFile) {
		this.validationCompchemFile = validationCompchemFile;
	}

	/**
	 * Checks if is consistency compchem ontology file.
	 *
	 * @return true, if compchem ontology file is consistent
	 */
	public boolean isConsistencyCompchemOntologyFile() {
		return consistencyCompchemOntologyFile;
	}

	/**
	 * Sets the consistency compchem ontology file.
	 *
	 * @param consistencyCompchemOntologyFile the new consistency of compchem ontology file
	 */
	public void setConsistencyCompchemOntologyFile(boolean consistencyCompchemOntologyFile) {
		this.consistencyCompchemOntologyFile = consistencyCompchemOntologyFile;
	}
	
	/**
	 * Gets the uuid.
	 *
	 * @return the uuid
	 */
	public String getUuid() {
		return uuid;
	}

	/**
	 * Sets the uuid.
	 *
	 * @param uuid the new uuid
	 */
	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	public String getOwlFileName() {
		return owlFileName;
	}

	public void setOwlFileName(String owlFileName) {
		this.owlFileName = owlFileName;
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
        return  "UUID: " + getUuid() + "|"+ "Gaussian file name: " + getGaussianFileName() + " | "
                + "Validation of Compchem xml file: " + isValidationCompchemFile() + " | "
                + "Consistency of Compchem owl file :  " + isConsistencyCompchemOntologyFile();
    }
}