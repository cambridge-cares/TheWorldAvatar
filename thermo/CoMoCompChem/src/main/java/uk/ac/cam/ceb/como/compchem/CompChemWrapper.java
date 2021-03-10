package uk.ac.cam.ceb.como.compchem;

import java.util.List;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.cml.element.CMLParameter;
import org.xmlcml.cml.element.CMLParameterList;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLPropertyList;

/**
 * A wrapper to get the last job in the first job list
 * @author pb556
 */
public class CompChemWrapper {

	
    private final CompChem compchem;
    private final CMLModule jobList;
    private final CMLModule job;
    
    private CMLModule initialization = null;
    private CMLModule finalization = null;
    private CMLMolecule initialMolecule = null;
    private CMLMolecule finalMolecule = null;
    private CMLPropertyList propertyList = null;
    private CMLParameterList parameterList = null;

    public CompChemWrapper() {
        this(new CompChem());
    }

    /**
     * @author nk510
     * @param compchem
     * Takes the first jobList and the last job of the first job list. if element does not exist then it creates one.
     */
    
    public CompChemWrapper(CompChem compchem) {
        this.compchem = compchem;
        this.jobList = compchem.getFirstJobListModule();
        this.job = CompChemElementUtils.getOrAddLastJob(jobList);        
        if (job == null || jobList == null || compchem == null) {
        	
            throw new RuntimeException("a compchem element does not have a job module or is null.");
        }
        ensureCompChemStructure();
    }

    /**
     * 
     * @param compchem
     * @param job
     * Specifies the job your want to be working with.
     * 
     */
    
    public CompChemWrapper(CompChem compchem, CMLModule job) {
        this.compchem = compchem;
        this.jobList = (CMLModule) job.getParent();
        this.job = job;
        ensureCompChemStructure();
    }

    private void ensureCompChemStructure() {
        initialization = CompChemElementUtils.getOrAddInitialization(job);
        finalization = CompChemElementUtils.getOrAddFinalization(job);
        initialMolecule = CompChemElementUtils.getOrAddMolecule(initialization);
        finalMolecule = CompChemElementUtils.getOrAddMolecule(finalization);
        propertyList = CompChemElementUtils.getOrAddPropertyList(job);
        parameterList = CompChemElementUtils.getOrAddParameterList(job);
    }

    public final void addSIUnitNamespace() {
        compchem.addSIUnitNamespace();
    }

    public final void addNonSIUnitNamespace() {
        compchem.addNonSIUnitNamespace();
    }

    public final void addIUPACNamespace() {
        compchem.addIUPACNamespace();
    }

    public final void addGaussianNamespace() {
        compchem.addGaussianNamespace();
    }

    public final void addChemIDNamespace() {
        compchem.addChemIDNamespace();
    }

    public CompChem getCompchem() {
        return compchem;
    }

    public CMLModule getJobList() {
        return jobList;
    }

    public CMLModule getJob() {
        return job;
    }

    public CMLModule getFinalization() {
        return finalization;
    }

    public CMLModule getInitialization() {
        return initialization;
    }

    public CMLMolecule getInitialMolecule() {
        return initialMolecule;
    }

    public CMLMolecule getFinalMolecule() {
        return finalMolecule;
    }

    public CMLParameterList getParameterList() {
        return parameterList;
    }

    public CMLPropertyList getPropertyList() {
        return propertyList;
    }

    /**
     * @author pb556
     * Be careful! this get all nested properties
     * @return
     */
    public List<CMLProperty> getProperties() {
        return propertyList.getPropertyDescendants();
    }

    /**
     * @author pb556
     * Be careful! this get all nested parameters
     * @return
     */
    public List<CMLParameter> getParameters() {
        return parameterList.getParameterDescendants();
    }
}
