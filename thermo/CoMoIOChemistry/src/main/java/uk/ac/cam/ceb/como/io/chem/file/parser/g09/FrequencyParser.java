/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.chem.file.parser.g09;

import com.cmclinnovations.io.parser.ParserHelper;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.Archive;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.GaussianHelper;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.JobSection;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemElementUtils;
import uk.ac.cam.ceb.como.compchem.info.ComputedInfoImpl;
import uk.ac.cam.ceb.como.compchem.info.MolecularInfoImpl;
import uk.ac.cam.ceb.como.tools.parser.util.DoubleTool;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.log4j.Logger;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLPropertyList;
import org.xmlcml.cml.element.CMLTable;
import uk.ac.cam.ceb.como.chem.property.Displacement;
import uk.ac.cam.ceb.como.chem.property.DisplacementList;
import uk.ac.cam.ceb.como.chem.property.Vibrations;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.StringList;

/**
 *
 * @author pb556
 */

/**
 * 
 * @author nk510 This class implements methods for parsing data in a job
 *         section. It extracts Geometry, Frequency, Energies. The class extends
 *         GaussianParser class.
 */
public class FrequencyParser extends GaussianParser {

	protected double[] zpeAndThermal = null;
	protected Vibrations normalModes = null;
	private Logger logger = Logger.getLogger(FrequencyParser.class);

	@Override
	public void parseSection() throws Exception {
		JobSection jobsecGeom = this.getGeometryJobSection(this.jSections);
		JobSection jobsecFreq = this.getFrequencyJobSection(this.jSections);

		String archiveString = GaussianHelper.extractArchive(jobsecGeom);
		String archiveStringFreq = GaussianHelper.extractArchive(jobsecFreq);

		// parse archive
		Archive archive = this.parseArchive(archiveString);
		MolecularInfoImpl molecularInfo = this.getMolecularInfo(jobsecGeom);

		ComputedInfoImpl computedInfo = this.getComputedInfo(jobsecGeom);
		List<CMLMolecule> molecules = this.parseMolecules(jobsecGeom);

		// TODO
		zpeAndThermal = extractEnergies(jobsecFreq);

		this.normalModes = this.parseVibrationalFrequencies(this.parseRawVibrationalFrequencies(jobsecFreq));

		CMLModule jobMod = CompChemElementUtils.addJob(((CompChem) (this.obj)).getFirstJobListModule());
		this.extractInitModule(jobMod, archive, molecularInfo, computedInfo, molecules);
		this.extractFinalModule(jobMod, archive, molecularInfo, computedInfo, molecules);

	}

	protected double[] extractEnergies(JobSection jobsec) {

		double thermal = Double.parseDouble(ParserHelper.parseLine(
				Pattern.compile("\\s*Thermal correction to Enthalpy.*?=\\s*(" + DoubleTool.NUMBER_REGEX + ").*"),
				jobsec.get(jobsec.getLastMatchPosition(0,
						"\\s*Thermal correction to Enthalpy.*?=\\s*(" + DoubleTool.NUMBER_REGEX + ").*"))));
		double zpe = Double.parseDouble(ParserHelper.parseLine(
				Pattern.compile("\\s*Zero.*?-point correction.*?=\\s*(" + DoubleTool.NUMBER_REGEX
						+ ")\\s*\\(Hartree/Particle\\).*"),
				jobsec.get(jobsec.getLastMatchPosition(0, "\\s*Zero.*?-point correction.*?=\\s*("
						+ DoubleTool.NUMBER_REGEX + ")\\s*\\(Hartree/Particle\\).*"))));
		return new double[] { zpe, thermal };
	}

	protected MolecularInfoImpl getMolecularInfo(JobSection jobsecGeom, JobSection jobsecFreq) {
		MolecularInfoImpl molecularInfo = this.getMolecularInfo(jobsecGeom);
		molecularInfo.setRotationalSymmetryNumber(Integer.parseInt(ParserHelper.parseLine(
				Pattern.compile("\\s*Rotational symmetry number\\s+(\\d+)\\..*"),
				jobsecFreq.get(jobsecFreq.getFirstMatchPosition(0, "\\s*Rotational symmetry number\\s+(\\d+)\\..*")))));
		return molecularInfo;
	}

	protected JobSection getFrequencyJobSection(List<JobSection> listJobsec) {
		for (Iterator<JobSection> jit = this.jSections.iterator(); jit.hasNext();) {
			JobSection jobsec = jit.next();
			// parse job by checking each line to find the beginning of pattern
			for (int i = 0; i < jobsec.size(); i++) {
				String line = jobsec.get(i);
				if (line.startsWith(" Harmonic frequencies")) {
					return jobsec;
				}
			}

		}
		return null;
	}

	@Override
	public Archive parseArchive(String archiveStr) throws Exception {

		Matcher matcher = Archive.ARCHIVE_PATTERN.matcher(archiveStr);
		if (matcher.find()) {
			Archive arc = new Archive();
			arc.setJobType(matcher.group(1));
			arc.setMethod(matcher.group(2));
			arc.setBasis(matcher.group(3));
			arc.setEmpiricalFormula(matcher.group(4));
			arc.setRoute(matcher.group(5));
			arc.setTitle(matcher.group(6));
			return arc;
		} else {
			logger.warn("Archive is not found for a given String : " + archiveStr);
			return null;
		}
	}

	@Override
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

		// List<CMLProperty> vibs = new ArrayList<CMLProperty>();
		List<CMLProperty> vibs = this.createCMLProperties(this.normalModes);

		for (CMLProperty pv : vibs) {
			property_list.addProperty(pv);
		}
		property_list.addProperty(chelper.createCMLPropertyScalar(computedInfo.getFinalSCFEnergyInHartree(),
				"nonSi:hartree", "cc:scfenergy", CompChem.COMPCHEM_NS));

		if (zpeAndThermal != null && zpeAndThermal.length == 2) {
			property_list.addProperty(
					chelper.createCMLPropertyScalar(zpeAndThermal[0], "nonSi:hartree", "cc:zpe", CompChem.COMPCHEM_NS));
			property_list.addProperty(chelper.createCMLPropertyScalar(zpeAndThermal[1], "nonSi:hartree",
					"cc:thermalenergy", CompChem.COMPCHEM_NS));
		}

		// TODO

		// add thermal
		// add zpe

		property_list.addProperty(chelper.createCMLPropertyScalar(molecularInfo.getRotationalSymmetryNumber(),
				"si:none", "cc:rotational_symmetry", CompChem.COMPCHEM_NS));

		return finalMod;
	}

	protected List<String> parseRawVibrationalFrequencies(JobSection jobsec) {

		if (jobsec != null) {
			for (int i = 0; i < jobsec.size(); i++) {
				String line = jobsec.get(i);
				if (line.startsWith(" Harmonic frequencies")) {
					// find and check position to extract
					int freqPos = jobsec.getFirstMatchPosition(i, " Frequencies --.*");
					int endPos = jobsec.getFirstMatchPosition(i, " -----*.*", " - Thermochemistry -.*");
					if (!(freqPos > 0 && endPos > freqPos)) {
						continue;
					}
					/**
					 * @author nk510 Print lines in jobsec.
					 */

					System.out.println("Job section line: " + line);

					// extract just the frequencies to be parsed
					return jobsec.subList(freqPos - 2, endPos); // assume that freq-2 !< 0
				}
			}
		}
		return null;
	}

	/**
	 * 
	 * @param rawFreqs
	 * @return
	 * @throws Exception
	 * 
	 */
	protected Vibrations parseVibrationalFrequencies(final List<String> rawFreqs) throws Exception {

		StringList sl = new StringList(rawFreqs);

		int from_line = 0;
		int pos = 0;
		int indexCounter = 1;
		int size = 0;
		// temporary storages
		Vibrations normalModes = new Vibrations();
		HashMap<Integer, DisplacementList> dispArrayList = new HashMap<>();
		// normalModes.setUnits("nonSi:cm^-1", "nonSi:amu", "nonSi:A^4/amu",
		// "nonSi:mDyne/A^-1", "nonSi:Angstroms");
		while ((pos = sl.getFirstMatchPosition(from_line, "\\s*Frequencies\\s+--.*")) > -1) {
			int atomHeaderPos = sl.getFirstMatchPosition(pos, "\\s*Atom\\s+AN.*");
			int displacementSectionEnd = sl.getFirstMatchPosition(atomHeaderPos + 1, "\\s*Frequencies\\s+--.*") - 2;
			if (displacementSectionEnd < 0) {
				displacementSectionEnd = rawFreqs.size();
			}
			// int local_num_freqs = 0;

			List<String> infos = sl.subList(pos, atomHeaderPos);
			// parse frequency info
			for (String line : infos) {
				if (line.contains("Frequencies")) {
					final String freqs = line.replaceAll("^.+?--", "");
					final List<Double> local_fs = ParserHelper.parseDoubles(ParserHelper.tokenize(freqs));
					size = local_fs.size();
					// number of frequencies interface each section
					// local_num_freqs = local_fs.size();
					normalModes.addAll(Vibrations.Property.FREQUENCY, local_fs);
				} else if (line.contains("Red. masses")) {
					final String rms = line.replaceAll("^.+?--", "");
					final List<Double> local_rms = ParserHelper.parseDoubles(ParserHelper.tokenize(rms));
					normalModes.addAll(Vibrations.Property.REDUCED_MASS, normalModes.size() - local_rms.size(),
							local_rms);
				} else if (line.contains("Frc consts")) {
					final String fcs = line.replaceAll("^.+?--", "");
					final List<Double> local_fcs = ParserHelper.parseDoubles(ParserHelper.tokenize(fcs));
					normalModes.addAll(Vibrations.Property.FORCE_CONSTANT, normalModes.size() - local_fcs.size(),
							local_fcs);
				} else if (line.contains("IR Inten")) {
					final String irs = line.replaceAll("^.+?--", "");
					final List<Double> local_irs = ParserHelper.parseDoubles(ParserHelper.tokenize(irs));
					normalModes.addAll(Vibrations.Property.IR_INTEN, normalModes.size() - local_irs.size(), local_irs);
				}
			}

			List<String> displacement = sl.subList(atomHeaderPos + 1, displacementSectionEnd);

			for (int i = 0; i < size; i++) {
				dispArrayList.put(indexCounter, new DisplacementList());
				indexCounter++;
			}

			// parse frequency info
			for (String line : displacement) {
				// extract the displacement information
				final List<Double> local_disp = ParserHelper.parseDoubles(ParserHelper.tokenize(line));
				if (local_disp.size() >= 5) {
					// extract the displacements and add them
					if ((local_disp.size() - 2) % 3 == 0) {
						for (int i = 0; i < size; i++) {
							double[] d = new double[3];
							Displacement xyz = new Displacement();
							xyz.setdx(local_disp.get(2 + i * 3));
							xyz.setdy(local_disp.get(2 + i * 3 + 1));
							xyz.setdz(local_disp.get(2 + i * 3 + 2));
							xyz.setAtomReference(
									Integer.toString(dispArrayList.get(indexCounter - size + i).size() + 1));

							// add to the list and to the hash map!
							dispArrayList.get(indexCounter - size + i).add(xyz);
						}
					}
				}
			}

			// update indexes
			pos = atomHeaderPos;
			from_line = atomHeaderPos;
		}

		List<Double> mode = new ArrayList<Double>();
		for (int i = 0; i < normalModes.size(); i++) {
			mode.add(i + 1.0);
			// List<VibrationalNormalMode.XYZ> p = dispArrayList.get(i + 1);
			normalModes.get(i).setDisplacements(dispArrayList.get(i + 1));
		}
		normalModes.addAll(Vibrations.Property.MODE, 0, mode);

		return normalModes;
	}

	protected List<CMLProperty> createCMLProperties(Vibrations normalModes) {

		List<CMLProperty> properties = new ArrayList<CMLProperty>();
		// This section is to create a group properties of vibrations
		CMLTable table = new CMLTable();
		// force constants
		String lbqname = chelper.createQName("cc:labels", CompChem.COMPCHEM_NS);
		List<String> labels = new ArrayList<String>();
		for (int i = 0; i < normalModes.size(); i++) {
			labels.add("f" + (i + 1));
		}
		CMLArray lb_array = chelper.createCMLArray(labels);
		lb_array.setDictRef(lbqname);
		table.appendChild(lb_array);

		// frequencies
		String fqname = chelper.createQName("cc:frequencies", CompChem.COMPCHEM_NS);
		CMLArray f_array = chelper.createCMLArray(normalModes.get(Vibrations.Property.FREQUENCY), "nonSi:cm^-1",
				CompChem.NONSI_NS);
		f_array.setDictRef(fqname);
		table.appendChild(f_array);

		// reducemass
		String rmqname = chelper.createQName("cc:reduced_masses", CompChem.COMPCHEM_NS);
		CMLArray redmass_array = chelper.createCMLArray(normalModes.get(Vibrations.Property.REDUCED_MASS), "nonSi:amu",
				CompChem.NONSI_NS);
		redmass_array.setDictRef(rmqname);
		table.appendChild(redmass_array);

		// raman scattering
		String rsqname = chelper.createQName("cc:ir_internal", CompChem.COMPCHEM_NS);
		CMLArray raman_array = chelper.createCMLArray(normalModes.get(Vibrations.Property.IR_INTEN), "nonSi:A^4/amu",
				CompChem.NONSI_NS);
		raman_array.setDictRef(rsqname);
		table.appendChild(raman_array);

		// force constants
		String fcqname = chelper.createQName("cc:force_consts", CompChem.COMPCHEM_NS);
		CMLArray fc_array = chelper.createCMLArray(normalModes.get(Vibrations.Property.FORCE_CONSTANT),
				"nonSi:mDyne/A^-1", CompChem.NONSI_NS);
		fc_array.setDictRef(fcqname);
		table.appendChild(fc_array);

		CMLProperty vibs = chelper.createCMLProperty("cc:vibrations", CompChem.COMPCHEM_NS);
		vibs.appendChild(table);
		properties.add(vibs);

		CMLTable tableDisplacements = new CMLTable();

		String aqname = chelper.createQName("cc:atoms", CompChem.COMPCHEM_NS);
		List<String> atoms = new ArrayList<String>();
		for (int i = 0; i < normalModes.get(0).getDisplacements().size(); i++) {
			atoms.add("a" + normalModes.get(0).getDisplacements().get(i).getReference());
		}
		CMLArray a_array = chelper.createCMLArray(atoms);
		a_array.setDictRef(aqname);
		tableDisplacements.appendChild(a_array);

		for (int i = 0; i < normalModes.size(); i++) {
			String disxpqname = chelper.createQName("cc:displacement_f" + (i + 1) + "_x3", CompChem.COMPCHEM_NS);
			String disypqname = chelper.createQName("cc:displacement_f" + (i + 1) + "_y3", CompChem.COMPCHEM_NS);
			String diszpqname = chelper.createQName("cc:displacement_f" + (i + 1) + "_z3", CompChem.COMPCHEM_NS);
			double[] xCoordinates = new double[normalModes.get(i).getDisplacements().size()];
			double[] yCoordinates = new double[normalModes.get(i).getDisplacements().size()];
			double[] zCoordinates = new double[normalModes.get(i).getDisplacements().size()];
			for (int j = 0; j < normalModes.get(i).getDisplacements().size(); j++) {
				xCoordinates[j] = normalModes.get(i).getDisplacements().get(j).dx();
				yCoordinates[j] = normalModes.get(i).getDisplacements().get(j).dy();
				zCoordinates[j] = normalModes.get(i).getDisplacements().get(j).dz();
			}
			CMLArray dx_array = chelper.createCMLArray(xCoordinates, "si:none", CompChem.SI_NS);
			CMLArray dy_array = chelper.createCMLArray(yCoordinates, "si:none", CompChem.SI_NS);
			CMLArray dz_array = chelper.createCMLArray(zCoordinates, "si:none", CompChem.SI_NS);

			dx_array.setDictRef(disxpqname);
			tableDisplacements.appendChild(dx_array);
			dy_array.setDictRef(disypqname);
			tableDisplacements.appendChild(dy_array);
			dz_array.setDictRef(diszpqname);
			tableDisplacements.appendChild(dz_array);
		}

		CMLProperty disps = chelper.createCMLProperty("cc:vibrational_displacements", CompChem.COMPCHEM_NS);
		disps.appendChild(tableDisplacements);
		properties.add(disps);

		return properties;
	}

	@Override
	public void clear() throws Exception {
		this.obj = null;
	}

	@Override
	public Object get() throws Exception {
		return obj;
	}
}
