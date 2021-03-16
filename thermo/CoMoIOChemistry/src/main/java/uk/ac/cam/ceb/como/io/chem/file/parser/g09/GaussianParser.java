/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package uk.ac.cam.ceb.como.io.chem.file.parser.g09;

import com.cmclinnovations.io.parser.ParserHelper;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.Archive;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.JobSection;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemElementUtils;
import uk.ac.cam.ceb.como.compchem.ContainerHelper;
import uk.ac.cam.ceb.como.compchem.info.BasicInfoImpl;
import uk.ac.cam.ceb.como.compchem.info.ComputedInfoImpl;
import uk.ac.cam.ceb.como.compchem.info.MolecularInfoImpl;
import uk.ac.cam.ceb.como.tools.parser.util.DoubleTool;
import static uk.ac.cam.ceb.como.tools.parser.util.DoubleTool.parseDouble;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import static java.lang.Integer.parseInt;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.log4j.Logger;
import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.cml.element.CMLParameter;
import org.xmlcml.cml.element.CMLParameterList;
import org.xmlcml.cml.element.CMLPropertyList;
import org.xmlcml.cml.tools.MoleculeTool;
import org.xmlcml.euclid.Point3;
import org.xmlcml.molutil.ChemicalElement;
import uk.ac.cam.ceb.como.io.chem.file.parser.ChemFileParser;

/**
 *
 * @author pb556
 */

public abstract class GaussianParser extends ChemFileParser {

	protected ContainerHelper chelper = null;
	protected List<JobSection> jSections = null;
	protected CompChem obj = null;

	private Logger logger = Logger.getLogger(GaussianParser.class);

	@Override
	public void parse() throws Exception {
		this.obj = this.createCompChem();
		this.chelper = new ContainerHelper((CompChem) this.obj);
		this.jSections = this.extractJobSections(this.f);		
		if (this.jSections.isEmpty()) {

			 throw new RuntimeException("Invalid Gaussian file");

		}

		this.parseSection();
	}	
	
	@Override
	public String toString()
	{
	  return ToStringBuilder.reflectionToString(this);
	}
	
	public List<JobSection> extractJobSections(File file) throws IOException {

		BufferedReader r = null;
		List<uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.JobSection> l_jobs = new ArrayList<uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.JobSection>();
		try {
			r = new BufferedReader(new FileReader(file));

			List<String> section_lines = new ArrayList<String>();

			int order = 0;

			String l;

			do {
				l = r.readLine();
				boolean ntg = false; // normal terminated
				if (l != null) {
					section_lines.add(l);
					ntg = l.contains("Normal termination of Gaussian");
				}
				if (ntg || l == null) { // end of section
					uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.JobSection js = new uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.JobSection(
							section_lines, order++, ntg);
					section_lines.clear();
					js.trim();
					if (!js.isEmpty()) {
						l_jobs.add(js);

					}
				}
			} while (l != null);
		} finally {
			IOUtils.closeQuietly(r);
		}
		return l_jobs;
	}

	/**
	 * Parse the Gaussian Archive by converting the archive section to one long
	 * String. It assumes that there is a GINC keywords in the beginning of line of
	 * the Gaussian Archive. This is actually unsafe. GINC (Gaussian Inc.) keyword
	 * can be changed (what I was told).
	 *
	 * @param js
	 * @return null if no archive is found otherwise archive is returned.
	 */
	public String extractArchive(JobSection js) {
		// Concatenate Archive to String
		int fia = -1; // first index of archive
		for (int i = 0; i < js.size(); i++) {
			if (js.get(i).matches("^ \\d\\\\\\d\\\\GINC.*")) {
				fia = i;
				break;
			}
		}

		if (fia >= 0) {
			String archive = "";
			for (int i = fia; i < js.size(); i++) {
				archive += js.get(i).substring(1);
				if (archive.endsWith("\\@")) {
					return archive;
				}
			}
			logger.warn("End of Job Section before the normal termination"
					+ " of Gaussian Archive. This should not happen unless" + " there is a bug in the codes.");
			return archive;
		} else {
			return null; // no archive found
		}

	}

	protected CMLModule extractInitModule(CMLModule jobModule, Archive archive, MolecularInfoImpl molecularInfo, ComputedInfoImpl computedInfo, List<CMLMolecule> molecules) {
		
		CMLModule initMod = CompChemElementUtils.getOrAddInitialization(jobModule);
		BasicInfoImpl basicInfo = (BasicInfoImpl) archive;

		if(molecules.isEmpty()) {}
		CMLMolecule molecule = molecules.get(0);
		
		molecule.setId("mol-init-" + 1);
		molecule.setFormalCharge(molecularInfo.getCharge());
		molecule.setSpinMultiplicity(molecularInfo.getSpinMultiplicity());
		initMod.appendChild(molecule);

		CMLParameterList parameter_list = new CMLParameterList();
		initMod.appendChild(parameter_list);

		{
			CMLParameter p = chelper.createCMLParameterScalar(basicInfo.getBasis(), "cc:basis", CompChem.COMPCHEM_NS);
			parameter_list.addParameter(p);
		}
		{
			CMLParameter p = chelper.createCMLParameterScalar(basicInfo.getMethod(), "cc:method", CompChem.COMPCHEM_NS);
			parameter_list.addParameter(p);
		}
		{
			CMLParameter p = chelper.createCMLParameterScalar(basicInfo.getJobType(), "g:goal", CompChem.GAUSSIAN_NS);
			parameter_list.addParameter(p);

			String goal = this.getGoal(archive);

			if (goal != null) {
				p = chelper.createCMLParameterScalar(goal, "cc:goal", CompChem.COMPCHEM_NS);
				parameter_list.addParameter(p);
			}
		}
		{
			CMLParameter p = chelper.createCMLParameterScalar(archive.getRoute(), "g:route", CompChem.GAUSSIAN_NS);
			parameter_list.addParameter(p);
		}
		return initMod;
	}

	protected CMLModule extractFinalModule(CMLModule jobModule, Archive archive, MolecularInfoImpl molecularInfo,
			ComputedInfoImpl computedInfo, List<CMLMolecule> molecules) {
		CMLModule finalMod = CompChemElementUtils.getOrAddFinalization(jobModule);

		CMLMolecule molecule = new CMLMolecule(molecules.get(molecules.size() - 1));
		molecule.setId("mol-final-" + 2);
		molecule.setFormalCharge(molecularInfo.getCharge());
		molecule.setSpinMultiplicity(molecularInfo.getSpinMultiplicity());
		finalMod.appendChild(molecule);

		CMLPropertyList property_list = new CMLPropertyList();
		finalMod.appendChild(property_list);

		property_list.addProperty(chelper.createCMLPropertyScalar(computedInfo.getFinalSCFEnergyInHartree(),
				"nonSi:hartree", "cc:scfenergy", CompChem.COMPCHEM_NS));
		property_list.addProperty(chelper.createCMLPropertyScalar(molecularInfo.getRotationalSymmetryNumber(),
				"si:none", "cc:rotational_symmetry", CompChem.COMPCHEM_NS));

		return finalMod;
	}

	protected String getGoal(Archive archive) {
		BasicInfoImpl basicInfo = (BasicInfoImpl) archive;
		String goal;
		if (basicInfo.getJobType().equalsIgnoreCase("FOpt")) {
			goal = "Full geometry optimization by Gaussian Program (i.e., that "
					+ "the variables including inactive variables are "
					+ "linearly independent and span the degrees of " + "freedom allowed by the molecular symmetry)";
		} else if (basicInfo.getJobType().equalsIgnoreCase("POpt")) {
			goal = "Partial geometry optimization by Gaussian Program (it also suppresses "
					+ "the frequency analysis at the end of optimizations "
					+ "which include second derivatives at every point.)";
		} else if (basicInfo.getJobType().equalsIgnoreCase("Opt")) {
			goal = "Geometry optimization";
		} else if (basicInfo.getJobType().equalsIgnoreCase("Freq")) {
			goal = "Frequency analysis";
		} else if (basicInfo.getJobType().equalsIgnoreCase("SP")) {
			goal = "Single point energy calculation";
		} else if (basicInfo.getJobType().equalsIgnoreCase("IRC")) {
			goal = "Reaction path following";
		} else if (basicInfo.getJobType().equalsIgnoreCase("IRCMax")) {
			goal = "Find the maximum energy along a specific reaction path";
		} else if (basicInfo.getJobType().equalsIgnoreCase("Scan")) {
			goal = "Potential energy surface scan";
		} else if (basicInfo.getJobType().equalsIgnoreCase("Polar")) {
			goal = "Polarizabilities or/and hyperpolarizabilities";
		} else if (basicInfo.getJobType().equalsIgnoreCase("Force")) {
			goal = "Calculate forces on the nuclei";
		} else if (basicInfo.getJobType().equalsIgnoreCase("Stable")) {
			goal = "Test wavefunction stability";
		} else if (basicInfo.getJobType().equalsIgnoreCase("Volume")) {
			goal = "Calculate molecular volume";
		} else {
			goal = null;
		}
		return goal;
	}

	public abstract Archive parseArchive(String archiveStr) throws Exception;

	protected abstract void parseSection() throws Exception;

	protected ComputedInfoImpl getComputedInfo(JobSection jobsec) {

		ComputedInfoImpl computedInfo = new ComputedInfoImpl();
		computedInfo.setFinalSCFEnergyInHartree(Double.parseDouble(ParserHelper.parseLine(
				Pattern.compile("\\s*SCF Done:.*?=\\s*(" + DoubleTool.NUMBER_REGEX + ")\\s+A\\.U\\.\\s+.*"),
				jobsec.get(jobsec.getLastMatchPosition(0,
						"\\s*SCF Done:.*?=\\s*(" + DoubleTool.NUMBER_REGEX + ")\\s+A\\.U\\.\\s+.*")))));
		return computedInfo;
	}

	protected MolecularInfoImpl getMolecularInfo(JobSection jobsec) {
		/**
		 * @author nk510 if() statement added if String jobsec is null.
		 */
		if (jobsec == null) {
			return null;
		}

		MolecularInfoImpl molecularInfo = new MolecularInfoImpl();
		molecularInfo.setCharge(Integer.parseInt(ParserHelper
				.parseLine(Pattern.compile("\\s*Charge =\\s*(-?\\d+)\\s*Multiplicity =\\s*\\d+\\s*.*"), jobsec.get(
						jobsec.getFirstMatchPosition(0, "\\s*Charge =\\s*(-?\\d+)\\s*Multiplicity =\\s*\\d+\\s*.*")))));
		molecularInfo.setMultiplicity(Integer.parseInt(ParserHelper
				.parseLine(Pattern.compile("\\s*Charge =\\s*-?\\d+\\s*Multiplicity =\\s*(\\d+)\\s*.*"), jobsec.get(
						jobsec.getFirstMatchPosition(0, "\\s*Charge =\\s*-?\\d+\\s*Multiplicity =\\s*(\\d+)\\s*.*")))));
		return molecularInfo;
	}

	protected CompChem createCompChem() {
		CompChem ccc = new CompChem();
		ccc.addChemIDNamespace();
		ccc.addGaussianNamespace();
		ccc.addIUPACNamespace();
		ccc.addNonSIUnitNamespace();
		ccc.addSIUnitNamespace();
		return ccc;
	}

	protected List<CMLMolecule> parseMolecules(JobSection jobsec) throws Exception {
		
		
		// parse job by checking each line to find the beginning of pattern
		List<CMLMolecule> molecules = new ArrayList<CMLMolecule>();
		for (int i = 0; i < jobsec.size(); i++) {
			String line = jobsec.get(i);
			if (line.contains("Input orientation:") || line.contains("Standard orientation:")
					|| line.contains("Z-Matrix orientation:")) {
				// find and check position to extract
				int fmpos = jobsec.getFirstMatchPosition(i, "^\\s+\\d+\\s+\\d+\\s+.*");
				if (fmpos < 0) {
					continue;
				}
				// extract just the coordinates to be parsed
				List<String> coords = jobsec.getLines(fmpos, "^\\s+\\d+\\s+\\d+\\s+.*");

				CMLMolecule mol = parseCoordinatesAndCalculateBonds(coords);
				molecules.add(mol);
				// update index. index must be on the same line of the last line
				// that match
				i = fmpos + coords.size() - ((coords.isEmpty()) ? 0 : 1);
			}
		}
		return molecules;
	}

	protected CMLMolecule parseCoordinatesAndCalculateBonds(final List<String> coords) throws Exception {
		if (coords.size() > 0) {
			CMLMolecule mol = chelper.createMolecule();
			for (int i = 0; i < coords.size(); i++) {
				List<String> str_list = ParserHelper.tokenize(coords.get(i));
				int lastIndex = str_list.size() - 1;
				// xyz coordinate
				double x = parseDouble(str_list.get(lastIndex - 2));
				double y = parseDouble(str_list.get(lastIndex - 1));
				double z = parseDouble(str_list.get(lastIndex));
				// create atom id
				String aid = "a" + str_list.get(0);
				try {
					CMLAtom atom = new CMLAtom(aid, ChemicalElement.getElement(parseInt(str_list.get(1))));
					atom.setPoint3(new Point3(x, y, z), CMLElement.CoordinateType.CARTESIAN);
					mol.addAtom(atom);
				} catch (Exception e) {
					throw new Exception(
							"Invalid chemical element has been identified with the Atom Number " + str_list.get(1));
				}
			}
			// calculate bonding
			MoleculeTool moleculeTool = MoleculeTool.getOrCreateTool(mol);
			moleculeTool.calculateBondedAtoms();
			moleculeTool.adjustBondOrdersToValency();
			return mol;
		} else {
			return null;
		}
	}

	protected JobSection getGeometryJobSection(List<JobSection> listJobsec) {
		for (Iterator<JobSection> jit = this.jSections.iterator(); jit.hasNext();) {
			JobSection jobsec = jit.next();
			// parse job by checking each line to find the beginning of pattern
			for (int i = 0; i < jobsec.size(); i++) {
				String line = jobsec.get(i);
				if (line.contains("Input orientation:") || line.contains("Standard orientation:")
						|| line.contains("Z-Matrix orientation:")) {
					return jobsec;
				}
			}

		}
		return null;
	}
}
