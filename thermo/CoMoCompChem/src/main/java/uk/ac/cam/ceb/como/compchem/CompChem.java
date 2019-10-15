package uk.ac.cam.ceb.como.compchem;

import uk.ac.cam.ceb.como.compchem.xml.NamespaceUtils;
import java.util.ArrayList;
import java.util.List;
import nu.xom.Nodes;
import org.xmlcml.cml.base.CMLConstants;
import org.xmlcml.cml.base.CMLNamespace;
import org.xmlcml.cml.element.CMLModule;

/**
 *
 * @author pb556
 *  
 */

/**
 * 
 * @author nk510 This class extends CMLModule class. It allows one to parse
 *         different type of CML child elements such as Property, Array, etc.
 *
 */

public class CompChem extends CMLModule {

	public static final String serial = "1.0";
	public static final String NONSI_NS = "http://www.xml-cml.org/unit/nonSi/";
	public static final String SI_NS = "http://www.xml-cml.org/unit/si/";
	public static final String COMPCHEM_NS = "http://www.xml-cml.org/dictionary/compchem/";
	public static final String CONVENTION_NS = "http://www.xml-cml.org/convention/";
	public static final String GAUSSIAN_NS = "http://www.xml-cml.org/dictionary/compchem/gaussian/";

	public CompChem() {
		setSerial(serial);
		setConvention("convention:compchem");
		addNamespaceDeclaration("convention", CONVENTION_NS);
		addNamespaceDeclaration("cc", COMPCHEM_NS);
		addNamespaceDeclaration(CMLConstants.XSD_PREFIX, CMLConstants.XSD_NS);
		addNamespaceDeclaration(CMLConstants.CML_UNITS, CMLConstants.UNIT_NS);
	}

	public CompChem(CMLModule module) {
		super(module);
	}

	public final void addChemIDNamespace() {
		addNamespaceDeclaration("chemid", "http://www.xml-cml.org/chemid/");
	}

	public final void addNonSIUnitNamespace() {
		addNamespaceDeclaration("nonSi", NONSI_NS);
	}

	public final void addSIUnitNamespace() {
		addNamespaceDeclaration("si", SI_NS);
	}

	public final void addGaussianNamespace() {
		addNamespaceDeclaration("g", GAUSSIAN_NS);
	}

	public final void addIUPACNamespace() {
		addNamespaceDeclaration("iupac", "http://www.iupac.org/");
	}

	public CMLModule getFirstJobListModule() {
		return getJobListModules().get(0);
	}

	/**
	 * @author nk510
	 * @return Extracts job modules and saves it as List<CMLModule> data structure.
	 */
	public List<CMLModule> getJobListModules() {
		String conventionPrefix = NamespaceUtils.getPrefixForKnownNamespace(this, "convention", CONVENTION_NS);
		String compchemPrefix = NamespaceUtils.getPrefixForKnownNamespace(this, "cc", COMPCHEM_NS);
		String q = "/cml:module[@convention='" + conventionPrefix + ":compchem']" + "/cml:module[@dictRef='"
				+ compchemPrefix + ":jobList']";
		Nodes jobLists = query(q, CMLNamespace.CML_XPATH);
		List<CMLModule> jobListModules = new ArrayList<CMLModule>();
		if (jobLists.size() > 0) {
			for (int i = 0; i < jobLists.size(); i++) {
				CMLModule cmlModule = (CMLModule) jobLists.get(i);
				jobListModules.add(cmlModule);
			}
		} else {
			jobListModules.add(CompChemElementUtils.addJobList(this));
		}

		return jobListModules;

	}

}