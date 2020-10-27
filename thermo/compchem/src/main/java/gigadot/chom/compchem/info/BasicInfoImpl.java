package gigadot.chom.compchem.info;

/**
 *
 * @author wp214
 */
public class BasicInfoImpl implements BasicInfo {

    private String jobType;
    private String inchi;
    private String method;
    private String basis;
    private String empiricalFormula;

    @Override
    public void clear() {
    }

    @Override
    public String getInchi() {
        return inchi;
    }

    @Override
    public void setInchi(String inchi) {
        this.inchi = inchi;
    }

    @Override
    public String getEmpiricalFormula() {
        return empiricalFormula;
    }

    @Override
    public void setEmpiricalFormula(String empiricalFormula) {
        this.empiricalFormula = empiricalFormula;
    }

    @Override
    public String getBasis() {
        return basis;
    }

    @Override
    public void setBasis(String basis) {
        this.basis = basis;
    }

    @Override
    public String getMethod() {
        return method;
    }

    @Override
    public void setMethod(String method) {
        this.method = method;
    }

    @Override
    public String getJobType() {
        return jobType;
    }

    @Override
    public void setJobType(String jobType) {
        this.jobType = jobType;
    }
}
