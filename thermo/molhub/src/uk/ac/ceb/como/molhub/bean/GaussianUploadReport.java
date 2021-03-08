package uk.ac.ceb.como.molhub.bean;

// TODO: Auto-generated Javadoc
/**
 * The Class GaussianUploadReport.
 * 
 *  @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 */
public class GaussianUploadReport implements Cloneable{

	/** The UUID. */
	private String uuid;
	
	/** The gaussian file name. */
	private String gaussianFileName;
	
	private String owlFileName ;
	
	/** The validation compchem file. */
	private boolean validationCompchemFile;
	
	/** The consistency compchem ontology (OWL) file. */
	private boolean consistencyCompchemOntologyFile;
	
	private String comment;
	/**
	 * Instantiates a new Gaussian upload report.
	 */
	public GaussianUploadReport() {
		
	}
	
    /**
     * @param uuid the UUID
     * @param gaussianFileName the Gaussian file name
     * @param comment comment when OWL file is not generated.
     */
    public GaussianUploadReport(String uuid, String gaussianFileName, boolean consistencyCompchemOntologyFile, String comment) {
		
		this.uuid=uuid;
		this.gaussianFileName=gaussianFileName;
		this.consistencyCompchemOntologyFile=consistencyCompchemOntologyFile;
		this.comment=comment;

	}

    
	/**
	 * Constructor of a new Gaussian upload report.
	 *
	 * @param uuid the UUID
	 * @param gaussianFileName the Gaussian file name
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
	 * Gets the Gaussian file name.
	 *
	 * @return the Gaussian file name
	 */
	public String getGaussianFileName() {
		return gaussianFileName;
	}

	/**
	 * Sets the Gaussian file name.
	 *
	 * @param gaussianFileName the new Gaussian file name
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
	 * Checks if is consistency compchem ontology (OWL) file.
	 *
	 * @return true, if compchem ontology (OWL) file is consistent
	 */
	public boolean isConsistencyCompchemOntologyFile() {
		return consistencyCompchemOntologyFile;
	}

	/**
	 * Sets the consistency compchem ontology (OWL) file.
	 *
	 * @param consistencyCompchemOntologyFile the new consistency of compchem ontology (OWL) file
	 */
	public void setConsistencyCompchemOntologyFile(boolean consistencyCompchemOntologyFile) {
		this.consistencyCompchemOntologyFile = consistencyCompchemOntologyFile;
	}
	
	/**
	 * Gets the UUID.
	 *
	 * @return the UUID
	 */
	public String getUuid() {
		return uuid;
	}

	/**
	 * Sets the UUID.
	 *
	 * @param uuid the new UUID
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


	public String getComment() {
		return comment;
	}


	public void setComment(String comment) {
		this.comment = comment;
	}
}