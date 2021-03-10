package uk.ac.cam.ceb.como.compchem;

import uk.ac.cam.ceb.como.compchem.xml.NamespaceUtils;
import nu.xom.Nodes;
import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.cml.element.CMLParameter;
import org.xmlcml.cml.element.CMLParameterList;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLPropertyList;

/**
 *
 * @author pb556
 */
public class CompChemElementUtils {

    public static CMLModule addJob(CMLModule jobList) {
        if (hasDictRef(jobList, "jobList", CompChem.COMPCHEM_NS)) {
            return addModule(jobList, "job");
        } else {
            throw new RuntimeException("Cannot create job for non-jobList element.");
        }
    }

    public static CMLModule getLastJob(CMLModule jobList) {
        if (hasDictRef(jobList, "jobList", CompChem.COMPCHEM_NS)) {
            String prefixForCompChemNamespace = jobList.getPrefixForNamespace(CompChem.COMPCHEM_NS);
            Nodes jobNodes = jobList.cmlQuery("./cml:module[@dictRef='" + prefixForCompChemNamespace + ":job']");
            if (jobNodes.size() > 0) {
                return (CMLModule) jobNodes.get(jobNodes.size() - 1);
            } else {
                return null;
            }
        } else {
            throw new RuntimeException("given module is not a job list");
        }
    }

    public static CMLModule getOrAddLastJob(CMLModule jobList) {
        CMLModule lastJob = getLastJob(jobList);
        if (lastJob == null) {
            lastJob = addJob(jobList);
        }
        return lastJob;
    }

    public static CMLModule addJobList(CompChem cc) {
        String cc_convention = cc.getConvention();
        String convention = cc.getPrefixForNamespace(CompChem.CONVENTION_NS) + ":compchem";
        if (convention.equals(cc_convention)) {
            return addModule(cc, "jobList");
        } else {
            throw new RuntimeException("Cannot create jobList for non-CompChem element.");
        }
    }

    public static CMLModule getOrAddEnvironment(CMLModule job) {
        ensureJobModule(job);
        return getOrAddModule(job, "environment");
    }

    public static CMLModule getOrAddInitialization(CMLModule job) {
        ensureJobModule(job);
        return getOrAddModule(job, "initialization");
    }

    /**
     * Add
     * Current version  does not support this fully.
     *
     * @param job
     * @return
     */
    public static CMLModule addCalculation(CMLModule job) {
        ensureJobModule(job);
        return addModule(job, "calculation");
    }

    public static CMLModule getOrAddFinalization(CMLModule job) {
        ensureJobModule(job);
        return getOrAddModule(job, "finalization");
    }

    private static void ensureJobModule(CMLModule job) {
        if (!hasDictRef(job, "job", CompChem.COMPCHEM_NS)) {
            throw new RuntimeException("A job module is expected but a module[@dictRef='" + job.getDictRef() + "'] is found");
        }
    }

    /**
     * check if an element has a dictRef
     * @param el
     * @param namespace
     * @param dictName
     * @return
     */
    private static boolean hasDictRef(CMLElement el, String dictName, String namespace) {
        String el_dictRef = el.getAttributeValue("dictRef");
        String dictRef = el.getPrefixForNamespace(namespace) + ":" + dictName;
        return dictRef.equals(el_dictRef);
    }

    /**
     * add a compchem module, i.e. job, joblist, initialization, calculation, finalization environment.
     * @param mod
     * @param dict
     * @return
     */
    private static CMLModule addModule(CMLModule mod, String dictName) {
        String prefix = NamespaceUtils.getPrefixForKnownNamespace(mod, "cc", CompChem.COMPCHEM_NS);
        CMLModule submod = new CMLModule();
        submod.setDictRef(prefix + ":" + dictName);
        mod.appendChild(submod);
        return submod;
    }

    /**
     * get or add a compchem module, i.e. job, joblist, initialization, calculation, finalization environment.
     * multiple mod then the first is get.
     * @param mod
     * @param dictName
     * @return
     */
    private static CMLModule getOrAddModule(CMLModule mod, String dictName) {
        String prefix = NamespaceUtils.getPrefixForKnownNamespace(mod, "cc", CompChem.COMPCHEM_NS);
        // find dictRef module
        Nodes mods = mod.cmlQuery("./cml:module[@dictRef='" + prefix + ":" + dictName + "']");
        CMLModule submod = null;
        if (mods.size() > 0) {
            submod = (CMLModule) mods.get(0);
        } else {
            submod = new CMLModule();
            submod.setDictRef(prefix + ":" + dictName);
            mod.appendChild(submod);
        }
        return submod;
    }

    /**
     * get or add a parameter from/to the initialization. if parameter is not found, then add a new one. if multiple
     * parameter are found return the first one
     * @param job
     * @param dictRef
     * @param dictRefNamespace
     * @return
     */
    public static CMLParameter getOrAddParameter(CMLModule job, String dictRef, String dictRefNamespace) {
        CMLParameter parameter = getParameter(job, dictRef, dictRefNamespace);
        if (parameter == null) {
            parameter = addParameter(job, dictRef, dictRefNamespace);
        }
        return parameter;
    }

    /**
     * add a new parameter to the initialization without checking whether the same property exists
     * @param job
     * @param dictRef
     * @param dictRefNamespace
     * @return
     */
    public static CMLParameter addParameter(CMLModule job, String dictRef, String dictRefNamespace) {
        ensureJobModule(job);
        String ldictRef = NamespaceUtils.createQName(job, dictRef, dictRefNamespace);
        CMLParameter p = new CMLParameter();
        p.setDictRef(ldictRef);
        getOrAddParameterList(job).addParameter(p);
        return p;
    }

    /**
     * get a parameter from the initialization. if parameter is not found, return null. if multiple
     * parameter are found return the first one.
     * @param job
     * @param dictRef
     * @param dictRefNamespace
     * @return
     */
    public static CMLParameter getParameter(CMLModule job, String dictRef, String dictRefNamespace) {
        ensureJobModule(job);
        String ldictRef = NamespaceUtils.createQName(job, dictRef, dictRefNamespace);
        Nodes props = getOrAddParameterList(job).cmlQuery("./cml:parameter[@dictRef='" + ldictRef + "']");
        if (props.size() > 0) {
            return (CMLParameter) props.get(0);
        } else {
            return null;
        }
    }

    /**
     * get or add a parameter list to the initialization
     * @param job
     * @return
     */
    public static CMLParameterList getOrAddParameterList(CMLModule job) {
        ensureJobModule(job);
        CMLModule initialization = getOrAddInitialization(job);
        Nodes plists = initialization.cmlQuery("./cml:parameterList");
        if (plists.size() == 1) {
            return (CMLParameterList) plists.get(0);
        } else if (plists.size() == 0) {
            CMLParameterList plist = new CMLParameterList();
            initialization.appendChild(plist);
            return plist;
        } else {
            throw new RuntimeException("CompChem contains more than one parameterList in the initialization.");
        }
    }

    /**
     * add or get a property from the finalization return the first one found
     * @param job
     * @param dictRef
     * @param dictRefNamespace
     * @return
     */
    public static CMLProperty getOrAddProperty(CMLModule job, String dictRef, String dictRefNamespace) {
        CMLPropertyList plist = getOrAddPropertyList(job);
        String ldictRef = NamespaceUtils.createQName(job, dictRef, dictRefNamespace);
        Nodes props = plist.cmlQuery("./cml:property[@dictRef='" + ldictRef + "']");
        if (props.size() > 0) {
            return (CMLProperty) props.get(0);
        } else {
            CMLProperty p = new CMLProperty();
            p.setDictRef(ldictRef);
            plist.addProperty(p);
            return p;
        }
    }

    /**
     * get a property from the finalization. if property is not found, return null. if multiple
     * parameter are found return the first one.
     * @param job
     * @param dictRef
     * @param dictRefNamespace
     * @return
     */
    public static CMLProperty getProperty(CMLModule job, String dictRef, String dictRefNamespace) {
        ensureJobModule(job);
        String ldictRef = NamespaceUtils.createQName(job, dictRef, dictRefNamespace);
        Nodes props = getOrAddPropertyList(job).cmlQuery("./cml:property[@dictRef='" + ldictRef + "']");
        if (props.size() > 0) {
            return (CMLProperty) props.get(0);
        } else {
            return null;
        }
    }

    /**
     * add a property to the finalization without checking whether the same property exists
     * @param job
     * @param dictRef
     * @param dictRefNamespace
     * @return
     */
    public static CMLProperty addProperty(CMLModule job, String dictRef, String dictRefNamespace) {
        ensureJobModule(job);
        String ldictRef = NamespaceUtils.createQName(job, dictRef, dictRefNamespace);
        CMLProperty p = new CMLProperty();
        p.setDictRef(ldictRef);
        getOrAddPropertyList(job).addProperty(p);
        return p;
    }

    /**
     * get or add a property list from/to the finalization.
     * @param job
     * @return
     */
    public static CMLPropertyList getOrAddPropertyList(CMLModule job) {
        CMLModule finalization = getOrAddFinalization(job);
        Nodes plists = finalization.cmlQuery("./cml:propertyList");
        if (plists.size() == 1) {
            return (CMLPropertyList) plists.get(0);
        } else if (plists.size() == 0) {
            CMLPropertyList plist = new CMLPropertyList();
            finalization.appendChild(plist);
            return plist;
        } else {
            throw new RuntimeException("CompChem contains more than one propertyList in the finalization.");
        }
    }

    private static void ensureModInJob(CMLModule modInJob) {
        if (!(hasDictRef(modInJob, "initialization", CompChem.COMPCHEM_NS)
                || hasDictRef(modInJob, "finalization", CompChem.COMPCHEM_NS)
                || hasDictRef(modInJob, "calculation", CompChem.COMPCHEM_NS))) {
            throw new RuntimeException("An initialization or a calculation or a finalization module is expected but "
                    + "a module[@dictRef='" + modInJob.getDictRef() + "'] is found");
        }
    }

    public static CMLMolecule getMolecule(CMLModule modInJob) {
        ensureModInJob(modInJob);
        return (CMLMolecule) modInJob.getFirstCMLChild(CMLMolecule.TAG);
    }

    public static CMLMolecule getOrAddMolecule(CMLModule modInJob) {
        CMLMolecule cmlm = getMolecule(modInJob);
        if (cmlm == null) {
            cmlm = addMolecule(modInJob);
        }
        return cmlm;
    }

    public static CMLMolecule addMolecule(CMLModule modInJob) {
        ensureModInJob(modInJob);
        String prefix = NamespaceUtils.getPrefixForKnownNamespace(modInJob, "cc", CompChem.CONVENTION_NS);
        CMLMolecule mol = new CMLMolecule();
        mol.setConvention(prefix + ":molecular");
        modInJob.appendChild(mol);
        return mol;
    }
}
