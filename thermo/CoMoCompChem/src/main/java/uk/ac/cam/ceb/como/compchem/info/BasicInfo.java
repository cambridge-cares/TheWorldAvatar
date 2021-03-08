package uk.ac.cam.ceb.como.compchem.info;

/**
 *
 * @author pb556
 */
public interface BasicInfo extends Info {

    public String getInchi();

    public void setInchi(String inchi);

    /**
     * Get the value of empiricalFormula
     *
     * @return the value of empiricalFormula
     */
    public String getEmpiricalFormula();

    /**
     * Set the value of empiricalFormula
     *
     * @param empiricalFormula new value of empiricalFormula
     */
    public void setEmpiricalFormula(String empiricalFormula);

    /**
     * Get the value of basis
     *
     * @return the value of basis
     */
    public String getBasis();

    /**
     * Set the value of basis
     *
     * @param basis new value of basis
     */
    public void setBasis(String basis);

    /**
     * Get the value of method
     *
     * @return the value of method
     */
    public String getMethod();

    /**
     * Set the value of method
     *
     * @param method new value of method
     */
    public void setMethod(String method);

    /**
     * Get the value of jobType
     *
     * @return the value of jobType
     */
    public String getJobType();

    /**
     * Set the value of jobType
     *
     * @param jobType new value of jobType
     */
    public void setJobType(String jobType);
}
