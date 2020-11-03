package gigadot.chom.compchem.parser;

import gigadot.chom.compchem.info.ComputedInfoImpl;
import gigadot.chom.compchem.info.MolecularInfoImpl;
import gigadot.chom.compchem.parser.gaussian.util.Archive;
import gigadot.chom.compchem.info.BasicInfoImpl;
import gigadot.chom.compchem.parser.gaussian.util.JobSection;
import gigadot.chom.compchem.parser.util.StringList;
import org.xmlcml.cml.element.CMLParameter;
import org.xmlcml.cml.element.CMLTable;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.base.CMLElement.CoordinateType;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.tools.MoleculeTool;
import org.xmlcml.euclid.Point3;
import org.xmlcml.molutil.ChemicalElement;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.CompChemElementUtils;
import gigadot.chom.compchem.ContainerHelper;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.cml.element.CMLParameterList;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLPropertyList;
import static gigadot.chom.compchem.parser.gaussian.util.GaussianHelper.*;
import static gigatools.lite.math.DoubleTool.parseDouble;
import static java.lang.Integer.parseInt;

/**
 * A Gaussian 03 reader and parser for molecular quantum calculation output.
 *
 * @author Weerapong Phadungsukanan
 */
public class GaussianParser extends ChemFileParser {

    private CompChem cc = null;
    private ContainerHelper chelper = null;

    /**
     * Read and parse molecular quantum calculation output from Gaussian 03 and
     * store information in given JChemDocument object.
     *
     * unable to make this method any smaller because it is implemented for the
     * best performance. e.g. use of continue; to skip the loop.
     *
     * @see #read(String)
     */
    @Override
    public CompChem parse(String file) throws IOException {
        cc = createCompChem();
        chelper = new ContainerHelper(cc);

        CMLModule jobListMod = cc.getFirstJobListModule();
        int id = 1;

        List<JobSection> jobsecs = jobsecs = extractJobSections(new File(file));
        for (Iterator<JobSection> jit = jobsecs.iterator(); jit.hasNext();) {

            JobSection jobsec = jit.next();

            String archiveString = extractArchive(jobsec);
            if (archiveString == null) {
                // remove job section which does not have archive. This normally
                // happens when a Guassian file is append with text at the beginning
                // or the end.
                jit.remove();
                continue;
            }

            // parse archive
            Archive archive = parseArchive(archiveString);
            BasicInfoImpl basicInfo = (BasicInfoImpl) archive;
            MolecularInfoImpl molecularInfo = new MolecularInfoImpl();
            ComputedInfoImpl computedInfo = new ComputedInfoImpl();

            List<CMLMolecule> molecules = new ArrayList<CMLMolecule>();
            List<CMLProperty> vibs = new ArrayList<CMLProperty>();

            // parse job by checking each line to find the beginning of pattern
            for (int i = 0; i < jobsec.size(); i++) {
                String line = jobsec.get(i);
                if (line.contains("Input orientation:") || line.contains("Standard orientation:") || line.contains("Z-Matrix orientation:")) {
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
                } else if (line.startsWith(" Harmonic frequencies")) {
                    // find and check position to extract
                    int freqPos = jobsec.getFirstMatchPosition(i, " Frequencies --.*");
                    int endPos = jobsec.getFirstMatchPosition(i, " -----*.*", " - Thermochemistry -.*");
                    if (!(freqPos > 0 && endPos > freqPos)) {
                        continue;
                    }
                    // extract just the frequencies to be parsed
                    List<String> rawFreqs = jobsec.subList(freqPos - 2, endPos); // assume that freq-2 !< 0
                    vibs = parseVibrations(rawFreqs);
                    // update the index to the last line that has been parsed
                    i = endPos - 1;
                } else {
                    // use short circuit evaluation instead of else if
                    boolean parseInline =
                            parseInlineChargeAndSpinMultiplicity(line, molecularInfo)
                            || parseInlineRotationalSymmetry(line, molecularInfo)
                            || parseInlineSCFDone(line, computedInfo);
                }
            }

            CMLModule jobMod = CompChemElementUtils.addJob(jobListMod);

            { // init module
                CMLModule initMod = CompChemElementUtils.getOrAddInitialization(jobMod);

                CMLMolecule molecule = molecules.get(0);
                molecule.setId("mol-init-" + id++);
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

                    String goal = null;
                    if (basicInfo.getJobType().equalsIgnoreCase("FOpt")) {
                        goal = "Full geometry optimization by Gaussian Program (i.e., that "
                                + "the variables including inactive variables are "
                                + "linearly independent and span the degrees of "
                                + "freedom allowed by the molecular symmetry)";
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
                    if (goal != null) {
                        p = chelper.createCMLParameterScalar(goal, "cc:goal", CompChem.COMPCHEM_NS);
                        parameter_list.addParameter(p);
                    }
                }
                {
                    CMLParameter p = chelper.createCMLParameterScalar(archive.getRoute(), "g:route", CompChem.GAUSSIAN_NS);
                    parameter_list.addParameter(p);
                }
            }

            { // final module
                CMLModule finalMod = CompChemElementUtils.getOrAddFinalization(jobMod);

                CMLMolecule molecule = new CMLMolecule(molecules.get(molecules.size() - 1));
                molecule.setId("mol-final-" + id++);
                molecule.setFormalCharge(molecularInfo.getCharge());
                molecule.setSpinMultiplicity(molecularInfo.getSpinMultiplicity());
                finalMod.appendChild(molecule);

                CMLPropertyList property_list = new CMLPropertyList();
                finalMod.appendChild(property_list);

                {
                    CMLProperty p = chelper.createCMLPropertyScalar(computedInfo.getFinalSCFEnergyInHartree(), "nonSi:hartree", "cc:scfenergy", CompChem.COMPCHEM_NS);
                    property_list.addProperty(p);
                    for (CMLProperty pv : vibs) {
                        property_list.addProperty(pv);
                    }
                }
                {
                    CMLProperty p = chelper.createCMLPropertyScalar(molecularInfo.getRotationalSymmetryNumber(), "si:none", "cc:rotational_symmetry", CompChem.COMPCHEM_NS);
                    property_list.addProperty(p);
                }
            }
        }
        if (jobsecs.isEmpty()) {
            throw new RuntimeException("Invalid Gaussian file");
        }

        return cc;
    }

    private CompChem createCompChem() {
        CompChem ccc = new CompChem();
        ccc.addChemIDNamespace();
        ccc.addGaussianNamespace();
        ccc.addIUPACNamespace();
        ccc.addNonSIUnitNamespace();
        ccc.addSIUnitNamespace();
        return ccc;
    }

    /**
     *
     * @param coords must be the format use by Gaussian
     * @return null if it cannot parse
     */
    private CMLMolecule parseCoordinatesAndCalculateBonds(final List<String> coords) {
        if (coords.size() > 0) {
            CMLMolecule mol = chelper.createMolecule();
            for (int i = 0; i < coords.size(); i++) {
                List<String> str_list = tokenize(coords.get(i));
                int lastIndex = str_list.size() - 1;
                // xyz coordinate
                double x = parseDouble(str_list.get(lastIndex - 2));
                double y = parseDouble(str_list.get(lastIndex - 1));
                double z = parseDouble(str_list.get(lastIndex));
                // create atom id
                String aid = "a" + str_list.get(0);
                CMLAtom atom = new CMLAtom(aid, ChemicalElement.getElement(parseInt(str_list.get(1))));
                atom.setPoint3(new Point3(x, y, z), CoordinateType.CARTESIAN);
                mol.addAtom(atom);
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

    private class XYZ {

        public double x;
        public double y;
        public double z;
    }

    private class FreqMode {

        final List<XYZ> coord = new ArrayList<XYZ>();
    }

    private List<CMLProperty> parseVibrations(final List<String> rawFreqs) throws IOException {
        StringList sl = new StringList(rawFreqs);
        int from_line = 0;
        int pos = 0;
        // temporary storages
        final List<Double> f_values = new ArrayList<Double>();
        final List<Double> reduceMass_values = new ArrayList<Double>();
        final List<Double> fc_values = new ArrayList<Double>();
        final List<Double> IRInten_vlaues = new ArrayList<Double>();
        List<FreqMode> vibModes = new ArrayList<FreqMode>();
        while ((pos = sl.getFirstMatchPosition(from_line, " Frequencies --.*")) > -1) {
            int atomHeaderPos = sl.getFirstMatchPosition(pos, " Atom AN.*");
            int local_num_freqs = 0;

            List<String> infos = sl.subList(pos, atomHeaderPos);
            // parse frequency info
            for (String line : infos) {

                if (line.startsWith(" Frequencies")) {
                    final String freqs = line.replaceAll("^.+?--", "");
                    final List<Double> local_fs = parseDoubles(tokenize(freqs));
                    // number of frequencies interface each section
                    local_num_freqs = local_fs.size();
                    f_values.addAll(local_fs);
                } else if (line.startsWith(" Red. masses")) {
                    final String rms = line.replaceAll("^.+?--", "");
                    final List<Double> local_rms = parseDoubles(tokenize(rms));
                    reduceMass_values.addAll(local_rms);
                } else if (line.startsWith(" Frc consts")) {
                    final String fcs = line.replaceAll("^.+?--", "");
                    final List<Double> local_fcs = parseDoubles(tokenize(fcs));
                    fc_values.addAll(local_fcs);
                } else if (line.startsWith(" IR Inten")) {
                    final String irs = line.replaceAll("^.+?--", "");
                    final List<Double> local_irs = parseDoubles(tokenize(irs));
                    IRInten_vlaues.addAll(local_irs);
                }
            }

            final String NUMBER_REGEX = "(?:[+-]?\\d+\\.\\d+)";
            // size
            List<String> eigens = sl.getLines(atomHeaderPos + 1, "^\\s+\\d+\\s+\\d+\\s+"
                    + NUMBER_REGEX + "\\s+" + NUMBER_REGEX + "\\s+" + NUMBER_REGEX + ".*");
            // parse frequency eigens
            FreqMode[] modes = new FreqMode[local_num_freqs];
            for (int j = 0; j < local_num_freqs; j++) {
                modes[j] = new FreqMode();
            }

            for (String line : eigens) {
                // System.out.println("> " + line);
                List<String> disp_values = tokenize(line);
                for (int j = 0; j < local_num_freqs; j++) {
                    XYZ xyz = new XYZ();
                    int eindex = disp_values.size() - 3 * (local_num_freqs - j);
                    xyz.x = parseDouble(disp_values.get(eindex));
                    eindex = disp_values.size() - 3 * (local_num_freqs - j) + 1;
                    xyz.y = parseDouble(disp_values.get(eindex));
                    eindex = disp_values.size() - 3 * (local_num_freqs - j) + 2;
                    xyz.z = parseDouble(disp_values.get(eindex));
                    modes[j].coord.add(xyz);
                }
            }

            vibModes.addAll(Arrays.asList(modes));

            // update indexes
            pos = atomHeaderPos;
            from_line = atomHeaderPos;
        }

        // convert temporary storage to CMLProperty
        List<CMLProperty> properties = new ArrayList<CMLProperty>();

        {// This section is to create a group properties of vibrations
            CMLTable table = new CMLTable();
            // force constants
            String lbqname = chelper.createQName("cc:labels", CompChem.COMPCHEM_NS);
            List<String> labels = new ArrayList<String>();
            for (int i = 0; i < f_values.size(); i++) {
                labels.add("f" + (i + 1));
            }
            CMLArray lb_array = chelper.createCMLArray(labels);
            lb_array.setDictRef(lbqname);
            table.appendChild(lb_array);

            // frequencies
            String fqname = chelper.createQName("cc:frequencies", CompChem.COMPCHEM_NS);
            CMLArray f_array = chelper.createCMLArray(f_values, "nonSi:GHz", CompChem.NONSI_NS);
            f_array.setDictRef(fqname);
            table.appendChild(f_array);

            // reducemass
            String rmqname = chelper.createQName("cc:reduced_masses", CompChem.COMPCHEM_NS);
            CMLArray redmass_array = chelper.createCMLArray(reduceMass_values, "nonSi:amu", CompChem.NONSI_NS);
            redmass_array.setDictRef(rmqname);
            table.appendChild(redmass_array);

            // force constants
            String fcqname = chelper.createQName("cc:force_consts", CompChem.COMPCHEM_NS);
            CMLArray fc_array = chelper.createCMLArray(fc_values, "nonSi:mDyne.A^-1", CompChem.NONSI_NS);
            fc_array.setDictRef(fcqname);
            table.appendChild(fc_array);

            CMLProperty vibs = chelper.createCMLProperty("cc:vibrations", CompChem.COMPCHEM_NS);
            vibs.appendChild(table);

            properties.add(vibs);
        }

        {// This section converts vibModes to CMLProperty
            // do nothing yet
        }

        return properties;
    }
}
