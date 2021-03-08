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
import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.compchem.info.ComputedInfoImpl;
import uk.ac.cam.ceb.como.compchem.info.MolecularInfoImpl;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLAtomArray;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLBondArray;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLPropertyList;
import uk.ac.cam.ceb.como.chem.property.Vibrations;

/**
 *
 * @author pb556
 */

public class GaussianHRParser extends FrequencyParser {

    @Override
    public void parseSection() throws Exception {

        JobSection jobsecGeom = this.getGeometryJobSection(this.jSections);
        JobSection jobsecFreq = this.getFrequencyJobSection(this.jSections);

        String archiveString = GaussianHelper.extractArchive(jobsecGeom);

        // parse archive
        Archive archive = this.parseArchive(archiveString);
        MolecularInfoImpl molecularInfo = this.getMolecularInfo(jobsecGeom);
        ComputedInfoImpl computedInfo = this.getComputedInfo(jobsecGeom);
        List<CMLMolecule> molecules = this.parseMolecules(jobsecGeom);
        CMLModule jobMod = CompChemElementUtils.addJob(((CompChem) (this.obj)).getFirstJobListModule());
        this.extractInitModule(jobMod, archive, molecularInfo, computedInfo, molecules);
        this.normalModes = this.parseVibrationalFrequencies(this.parseRawVibrationalFrequencies(jobsecFreq));

        this.extractFinalModule(jobMod, archive, molecularInfo, computedInfo, molecules);
    }

    @Override
    protected CMLModule extractFinalModule(CMLModule jobModule, Archive archive, MolecularInfoImpl molecularInfo, ComputedInfoImpl computedInfo, List<CMLMolecule> molecules) {
        CMLModule finalMod = CompChemElementUtils.getOrAddFinalization(jobModule);

        CMLMolecule molecule = new CMLMolecule(molecules.get(molecules.size() - 1));
        molecule.setId("mol-final-" + 2);
        molecule.setFormalCharge(molecularInfo.getCharge());
        molecule.setSpinMultiplicity(molecularInfo.getSpinMultiplicity());
        finalMod.appendChild(molecule);

        CMLPropertyList property_list = new CMLPropertyList();
        finalMod.appendChild(property_list);

        List<CMLProperty> vibs = this.createCMLProperties(this.normalModes);

        for (CMLProperty pv : vibs) {
            property_list.addProperty(pv);
        }
        property_list.addProperty(chelper.createCMLPropertyScalar(computedInfo.getFinalSCFEnergyInHartree(), "nonSi:hartree", "cc:scfenergy", CompChem.COMPCHEM_NS));
        property_list.addProperty(chelper.createCMLPropertyScalar(molecularInfo.getRotationalSymmetryNumber(), "si:none", "cc:rotational_symmetry", CompChem.COMPCHEM_NS));

        try {
            //List<CMLProperty> tors = this.parseHinderedRotors(this.getHRJobSection(this.jSections));
            //VibrationalNormalModes rotModes = this.identifyRotationalModes(this.getHRJobSection(this.jSections));
            List<TorsionalBond> torsionalBonds = this.identifyTorsionalBonds(this.getHRJobSection(this.jSections), this.identifyRotationalModes(this.getHRJobSection(this.jSections)));
            //CMLPropertyList bondArray = new CMLPropertyList();

            CMLProperty hrBonds = chelper.createCMLProperty("cc:hindered_rotations", CompChem.COMPCHEM_NS);

            for (int i = 0; i < torsionalBonds.size(); i++) {
                CMLPropertyList property_list_hr = new CMLPropertyList(); //chelper.creat("cc:hindered_rotations", CompChem.COMPCHEM_NS);
                property_list_hr.setDictRef("cc:bond_" + torsionalBonds.get(i).bond.getAtom(0).getId() + "_" + torsionalBonds.get(i).bond.getAtom(1).getId());

                property_list_hr.appendChild(chelper.createCMLPropertyScalar((int) torsionalBonds.get(i).mode.getMode(), "si:none", "cc:vibrational_mode", CompChem.COMPCHEM_NS));
                property_list_hr.appendChild(chelper.createCMLPropertyScalar((int) torsionalBonds.get(i).multiplicity, "si:none", "cc:multiplicity", CompChem.COMPCHEM_NS));
                property_list_hr.appendChild(chelper.createCMLPropertyScalar((int) torsionalBonds.get(i).periodicity, "si:none", "cc:periodicity", CompChem.COMPCHEM_NS));
                property_list_hr.appendChild(chelper.createCMLPropertyScalar((int) torsionalBonds.get(i).symmetry, "si:none", "cc:symmetry", CompChem.COMPCHEM_NS));
                
                // redundant information
                //property_list_hr.appendChild(chelper.createCMLPropertyScalar(torsionalBonds.get(i).mode.frcConst, "nonSi:mDyne/A^-1", "cc:force_const", CompChem.COMPCHEM_NS));
                //property_list_hr.appendChild(chelper.createCMLPropertyScalar(torsionalBonds.get(i).mode.freq, "nonSi:cm^-1", "cc:frequency", CompChem.COMPCHEM_NS));
                //property_list_hr.appendChild(chelper.createCMLPropertyScalar(torsionalBonds.get(i).mode.irInten, "nonSi:A^4/amu", "cc:ir_internal", CompChem.COMPCHEM_NS));
                //property_list_hr.appendChild(chelper.createCMLPropertyScalar(torsionalBonds.get(i).mode.redMass, "nonSi:amu", "cc:reduced_mass", CompChem.COMPCHEM_NS));
                

                property_list_hr.appendChild(chelper.createCMLPropertyScalar((double) torsionalBonds.get(i).freq, "nonSi:cm^-1", "cc:internal_rotational_frequency", CompChem.COMPCHEM_NS));
                property_list_hr.appendChild(chelper.createCMLPropertyScalar((double) torsionalBonds.get(i).redMoment, "nonSi:amu*Bohr^2", "cc:reduced_moment", CompChem.COMPCHEM_NS));

                List<String> atomArray = new ArrayList<String>();
                for (CMLAtom a : torsionalBonds.get(i).top) {
                    atomArray.add(a.getId());
                }

                CMLProperty propertyAtomArray = chelper.createCMLProperty("cc:top_composition", CompChem.COMPCHEM_NS);
                CMLArray lb_array = chelper.createCMLArray(atomArray);
                lb_array.setDictRef(chelper.createQName("cc:top_composition", CompChem.COMPCHEM_NS));

                propertyAtomArray.appendChild(lb_array);
                property_list_hr.appendChild(propertyAtomArray);
                //torsionalBonds.get(i).bond.appendChild(property_list_hr);
                //hrBonds.appendChild(torsionalBonds.get(i).bond);
                hrBonds.appendChild(property_list_hr);
            }
            //hrBonds.appendChild(bondArray);
            property_list.addProperty(hrBonds);

        } catch (Exception ex) {
            Logger.getLogger(GaussianHRParser.class.getName()).log(Level.SEVERE, null, ex);
        }

        return finalMod;
    }

    protected JobSection getHRJobSection(List<JobSection> listJobsec) {
        for (Iterator<JobSection> jit = this.jSections.iterator(); jit.hasNext();) {
            JobSection jobsec = jit.next();
            // parse job by checking each line to find the beginning of pattern
            for (int i = 0; i < jobsec.size(); i++) {
                String line = jobsec.get(i);
                if (line.contains("Hindered Internal Rotation Analysis")) {
                    return jobsec;
                }
            }

        }
        return null;
    }

    protected List<String> parseRawHinderedRotors(JobSection jobsec) throws Exception {
        for (int i = 0; i < jobsec.size(); i++) {
            String line = jobsec.get(i);
            if (line.startsWith("\\s+Hindered Internal Rotation Analysis.*")) {
                // find and check position to extract
                int hrPos = jobsec.getFirstMatchPosition(i, "\\s*Internal coordinate list sorted.*");
                int endPos = jobsec.getFirstMatchPosition(i, " -----*.*", " - Thermochemistry -.*");
                if (!(hrPos > 0 && endPos > hrPos)) {
                    continue;
                }
                // extract just the frequencies to be parsed
                return jobsec.subList(hrPos - 2, endPos); // assume that freq-2 !< 0
            }
        }

        return null;
    }

    //  Identified internal rotation modes:
    private Vibrations identifyRotationalModes(JobSection jobsec) {
        Vibrations rotModes = new Vibrations();
//        rotModes.setUnits("nonSi:cm^-1", "nonSi:amu", "nonSi:A^4/amu", "nonSi:mDyne/A^-1", "nonSi:Angstroms");
        int indexThermochemistry = jobsec.getFirstMatchPosition(0, " -----*.*", " - Thermochemistry -.*");
        if (indexThermochemistry != -1) {
            int startPos = jobsec.getFirstMatchPosition(indexThermochemistry, "\\s*Identified internal rotation modes:.*");
            int endPos = jobsec.getFirstMatchPosition(indexThermochemistry, "\\s*Vibrational temperatures:.*");
            if (startPos == -1 || endPos == -1 || endPos < startPos) {
                return null;
            }
            List<Integer> modes = ParserHelper.extractIntegers(jobsec.subList(startPos + 1, endPos));
            for (Integer i : modes) {
                rotModes.add(this.normalModes.get(i - 1));
            }
        }
        return rotModes;
    }

    private List<TorsionalBond> identifyTorsionalBonds(JobSection jobsec, Vibrations rotModes) throws Exception {
        List<TorsionalBond> torBonds = new ArrayList<TorsionalBond>();
        int startPos = jobsec.getFirstMatchPosition(0, "\\s+Rotor\\s+Bond\\s+Periodicity\\s+Symmetry\\s+Number\\s+Multiplicity.*");
        if (startPos != -1) {
            int endPos = jobsec.getFirstMatchPosition(startPos, "\\s*Normal Mode Analysis for Internal Rotation.*");
            if (endPos == -1 || endPos < startPos) {
                return null;
            }
            // parse whole table line by line
            List<Integer> properties = ParserHelper.extractIntegers(jobsec.subList(startPos + 1, endPos));
            if (properties.size() % 6 != 0) {
                return null;
            }
            int j = 0;
            for (int i = 0; i < properties.size(); i++) {
                switch (i % 6) {
                    case 0:
                        torBonds.add(new TorsionalBond());
                        torBonds.get(j).mode = rotModes.get(j);
                        break;
                    case 1:
                        torBonds.get(j).bond = this.identifyBond("a" + properties.get(i).toString(), "a" + properties.get(i + 1).toString());
                        torBonds.get(j).top = this.identifyTop(jobsec, torBonds.get(j).bond);
                        break;
                    case 2:
                        break;
                    case 3:
                        torBonds.get(j).periodicity = properties.get(i);
                        break;
                    case 4:
                        torBonds.get(j).symmetry = properties.get(i);
                        break;
                    case 5:
                        torBonds.get(j).multiplicity = properties.get(i);
                        j++;
                        break;
                    default:
                        return null;
                }
            }

            int startPosFreq = jobsec.getFirstMatchPosition(endPos, "\\s+Frequencies\\s+---.*");
            int startPosRedMom = jobsec.getFirstMatchPosition(endPos, "\\s+Reduced\\s+Moments\\s+---.*");
            
            int indexEnd = jobsec.getFirstMatchPosition(0, " -----*.*", " - Thermochemistry -.*");
            
            List<Double> freq = new ArrayList<Double>();
            List<Double> redMom = new ArrayList<Double>();
            
            while (startPosFreq != -1 && startPosRedMom != -1) {
                freq.addAll(ParserHelper.extractDoubles(jobsec.subList(startPosFreq, startPosFreq + 1)));
                redMom.addAll(ParserHelper.extractDoubles(jobsec.subList(startPosRedMom, startPosRedMom + 1)));

                startPosFreq = jobsec.getFirstMatchPosition(startPosFreq + 1, "\\s+Frequencies\\s+---.*");
                startPosRedMom = jobsec.getFirstMatchPosition(startPosRedMom + 1, "\\s+Reduced\\s+Moments\\s+---.*");
                
                if (indexEnd < startPosFreq || indexEnd < startPosRedMom) {
                    break;
                }
            }
            
            if (freq.size() != redMom.size() || freq.size() != torBonds.size()) {
                throw new Exception("Internal rotational frequencies and reduced moments cannot be extracted.");
            }
            for (int i = 0; i < torBonds.size(); i++) {
                torBonds.get(i).freq = freq.get(i);
                torBonds.get(i).redMoment = redMom.get(i);
            }

        }
        return torBonds;
    }

    private List<CMLAtom> identifyTop(JobSection jobsec, CMLBond bond) {
        //Identification of rotating group for bond      1 -      2
        int startPosSec = jobsec.getFirstMatchPosition(0, "\\s*Identification\\s+of\\s+rotating\\s+group\\s+for\\s+bond\\s+("
                + (bond.getAtom(0).getId().substring(1) + "|" + bond.getAtom(1).getId().substring(1)) + ")\\s+-\\s+("
                + (bond.getAtom(1).getId().substring(1) + "|" + bond.getAtom(0).getId().substring(1)) + ").*");
        if (startPosSec == -1) {
            return null;
        }
        int startPos = jobsec.getFirstMatchPosition(startPosSec, "\\s*Composition\\s+of\\s+rotating\\s+group\\s+:.*");
        int endPos = jobsec.getFirstMatchPosition(startPosSec, "\\s*Geometrical\\s+symmetry\\s+number\\s+=.*");
        if (startPos == -1 || endPos == -1 || startPos > endPos) {
            return null;
        }
        List<Integer> ids = ParserHelper.extractIntegers(jobsec.subList(startPos + 1, endPos));
        List<CMLAtom> top = new ArrayList<CMLAtom>();
        for (Integer id : ids) {
            top.add(this.identifyAtom("a" + id.toString()));
        }
        return top;
    }

    private CMLAtom identifyAtom(String atom) {
        CompChemWrapper ccw = new CompChemWrapper((CompChem) this.obj);
        CMLAtomArray atomArray = ccw.getInitialMolecule().getAtomArray();
        if (atomArray != null) {
            List<CMLAtom> atoms = atomArray.getAtoms();
            for (int i = 0; i < atoms.size(); i++) {
                if (((atoms.get(i).getId() == null ? atom == null : atoms.get(i).getId().equals(atom)))) {
                    return atoms.get(i);
                }
            }
        }
        return null;
    }

    private CMLBond identifyBond(String atom1, String atom2) {
        CompChemWrapper ccw = new CompChemWrapper((CompChem) this.obj);
        CMLBondArray bondArray = ccw.getInitialMolecule().getBondArray();
        if (bondArray != null) {
            List<CMLBond> bonds = bondArray.getBonds();
            for (int i = 0; i < bonds.size(); i++) {
                if (((bonds.get(i).getAtomId(0) == null ? atom1 == null : bonds.get(i).getAtomId(0).equals(atom1))
                        && (bonds.get(i).getAtomId(1) == null ? atom2 == null : bonds.get(i).getAtomId(1).equals(atom2)))
                        || ((bonds.get(i).getAtomId(1) == null ? atom1 == null : bonds.get(i).getAtomId(1).equals(atom1))
                        && (bonds.get(i).getAtomId(2) == null ? atom2 == null : bonds.get(i).getAtomId(2).equals(atom2)))) {
                    return bonds.get(i);
                }
            }
        }
        return null;
    }
    //  Harmonic frequencies (cm**-1), IR intensities (KM/Mole), Raman scattering
    //  activities (A**4/AMU), depolarization ratios for plane and unpolarized
    //  incident light, reduced masses (AMU), force constants (mDyne/A),
    //  and normal coordinates:
}
