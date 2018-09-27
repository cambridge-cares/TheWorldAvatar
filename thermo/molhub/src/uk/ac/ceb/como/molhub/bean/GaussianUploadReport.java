package uk.ac.ceb.como.molhub.bean;

public class GaussianUploadReport implements Cloneable{

	private String uuid;
	private String gaussianFileName;
	private boolean validationCompchemFile;
	private boolean consistencyCompchemOntologyFile;
	
	public GaussianUploadReport() {
		
	}
	
	public GaussianUploadReport(String uuid, String gaussianFileName, boolean validationCompchemFile, boolean consistencyCompchemOntologyFile) {
		
		this.uuid=uuid;
		this.gaussianFileName=gaussianFileName;
		this.validationCompchemFile=validationCompchemFile;
		this.consistencyCompchemOntologyFile=consistencyCompchemOntologyFile;		
	}

	public String getGaussianFileName() {
		return gaussianFileName;
	}

	public void setGaussianFileName(String gaussianFileName) {
		this.gaussianFileName = gaussianFileName;
	}

	public boolean isValidationCompchemFile() {
		return validationCompchemFile;
	}

	public void setValidationCompchemFile(boolean validationCompchemFile) {
		this.validationCompchemFile = validationCompchemFile;
	}

	public boolean isConsistencyCompchemOntologyFile() {
		return consistencyCompchemOntologyFile;
	}

	public void setConsistencyCompchemOntologyFile(boolean consistencyCompchemOntologyFile) {
		this.consistencyCompchemOntologyFile = consistencyCompchemOntologyFile;
	}
	
	public String getUuid() {
		return uuid;
	}

	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	@Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    @Override
    public String toString() {
        return  "UUID: " + getUuid() + "|"+ "Gaussian file name: " + getGaussianFileName() + " | "
                + "Validation of Compchem xml file: " + isValidationCompchemFile() + " | "
                + "Consistency of Compchem owl file :  " + isConsistencyCompchemOntologyFile();
    }
}