/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.rotation.hindered.io.cml;

import java.io.File;
import java.util.List;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.RotationalMode;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.RotationalModes;
import uk.ac.cam.ceb.como.rotation.hindered.io.MockSpecies;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleAtom;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleBond;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleRotMode;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleRotModes;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleSpecies;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleVibNormalMode;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleVibNormalModes;
import org.junit.Ignore;
import org.junit.Test;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.cml.element.CMLParameter;
import uk.ac.cam.ceb.como.chem.property.Vibration;
import uk.ac.cam.ceb.como.chem.property.Vibrations;
import uk.ac.cam.ceb.como.io.chem.file.parser.cml.CMLCompChemParser;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;

/**
 *
 * @author pb556
 */

public class CMLHRFileParserTest {

    @Test
    @Ignore
    public void getInitMoleculeTest_0232Diff() throws Exception {

    	IRCompChemWrapper hrCCW = getCompChemWrapper(new File("test_data/cml/freq-hr-fine-species-0232-radical-0-restricted_diff_added.cml"));

        // verify against read data
        assert (verify(hrCCW.getInitialMolecule(), MockSpecies.getSpecies0232Modified(), -1, 2));
    }

    @Test
    @Ignore
    public void getFinalMoleculeTest_0232Diff() throws Exception {
        IRCompChemWrapper hrCCW = getCompChemWrapper(new File("test_data/cml/freq-hr-fine-species-0232-radical-0-restricted_diff_added.cml"));

        // verify against read data
        assert (verify(hrCCW.getFinalMolecule(), MockSpecies.getSpecies0232(), 0, 1));
    }

    @Test
    @Ignore
    public void getInitMoleculeTest_0232() throws Exception {
        IRCompChemWrapper hrCCW = getCompChemWrapper(new File("test_data/cml/freq-hr-fine-species-0232-radical-0-restricted.cml"));

        // verify against read data
        assert (verify(hrCCW.getFinalMolecule(), MockSpecies.getSpecies0232(), 0, 1));
    }

    @Test
    @Ignore
    public void getFinalMoleculeTest_0232() throws Exception {
        IRCompChemWrapper hrCCW = getCompChemWrapper(new File("test_data/cml/freq-hr-fine-species-0232-radical-0-restricted.cml"));
        // verify against read data
        assert (verify(hrCCW.getFinalMolecule(), MockSpecies.getSpecies0232(), 0, 1));
    }

    protected boolean verify(CMLMolecule mol, SimpleSpecies species, int charge, int multiplicity) {
        boolean valid = true;
        valid &= verifyAtoms(species.getAtoms(), mol.getAtoms());
        valid &= verifyBonds(species.getBonds(), mol.getBonds());
        valid &= mol.getFormalCharge() == charge;
        valid &= mol.getSpinMultiplicity() == multiplicity;
        return valid;
    }

    protected boolean verifyAtoms(List<SimpleAtom> refAtoms, List<CMLAtom> atoms) {
        for (SimpleAtom refAtom : refAtoms) {
            boolean identified = false;
            for (CMLAtom atom : atoms) {
                if (refAtom.equals(atom)) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                return false;
            }
        }
        return true;
    }

    protected boolean verifyBonds(List<SimpleBond> refBonds, List<CMLBond> bonds) {
        for (SimpleBond refBond : refBonds) {
            boolean identified = false;
            for (CMLBond bond : bonds) {
                if (refBond.equals(bond)) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                return false;
            }
        }
        return true;
    }

    @Test
    @Ignore
    public void getInitParameterListTest_0232() throws Exception {
        IRCompChemWrapper hrCCW = getCompChemWrapper(new File("test_data/cml/freq-hr-fine-species-0232-radical-0-restricted.cml"));

        // verify parameter value
        assert (verifyParameterValue(hrCCW, "cc:basis", "6-311+G(d,p)"));
        assert (verifyParameterValue(hrCCW, "cc:method", "RB971"));
        assert (verifyParameterValue(hrCCW, "g:goal", "FOpt"));
        assert (verifyParameterValue(hrCCW, "cc:goal", "Full geometry optimization by Gaussian Program (i.e., that the variables including inactive variables are linearly independent and span the degrees of freedom allowed by the molecular symmetry)"));
        assert (verifyParameterValue(hrCCW, "g:route", "#p rB971/6-311+G(d,p) Opt=(Tight, NewEstmFC, MaxCyc=400) Freq=(ReadHinderedRotor) GFInput Population=None Integral(Grid=UltraFine) SCF=(MaxCyc=400, XQC) Guess=Mix NoSymmetry"));
    }

    protected boolean verifyParameterValue(IRCompChemWrapper hrCCW, String dictRef, String value) {
        List<CMLParameter> par = hrCCW.getParameterList().getParameterDescendants();
        if (par.isEmpty()) {
            return false;
        }
        for (CMLParameter p : par) {
            if (p.getDictRef().equalsIgnoreCase(dictRef)) {
                return p.getValue().trim().equalsIgnoreCase(value.trim());
            }
        }
        return false;
    }

    @Test
    @Ignore
    public void getRotationalModesTest_0232() throws Exception {
        IRCompChemWrapper hrCCW = getCompChemWrapper(new File("test_data/cml/freq-hr-fine-species-0232-radical-0-restricted.cml"));

        // verify vibrational modes
        RotationalModes modes = hrCCW.getRotationalModes();
        assert (modes.getFrequencyUnit().compareTo("nonSi:cm^-1") == 0);
        assert (modes.getReducedMomentUnit().compareTo("nonSi:amu*Bohr^2") == 0);
        
        SimpleRotModes refModes = MockSpecies.getSpecies0232RotationalModes();
        for (SimpleRotMode refMode : refModes) {
            boolean identified = false;
            for (RotationalMode mode : modes) {
                if (refMode.equals(mode)) {
                    identified = true;
                }
            }
            assert (identified);
        }
    }

    @Test
    @Ignore
    public void getVibrationalModesTest_0232() throws Exception {
        IRCompChemWrapper hrCCW = getCompChemWrapper(new File("test_data/cml/freq-hr-fine-species-0232-radical-0-restricted.cml"));

        // verify vibrational modes
        Vibrations modes = hrCCW.getVibrationalNormalModes();

        // verify units
//        assert (modes.getDisplacementUnit().equals("nonSi:Angstroms"));
//        assert (modes.getFrequencyUnit().equals("nonSi:cm^-1"));
//        assert (modes.getIRInternalUnit().equals("nonSi:A^4/amu"));
//        assert (modes.getReducedMassUnit().equals("nonSi:amu"));
//        assert (modes.getForceConstantUnit().equals("nonSi:mDyne/A^-1"));

        // verify values
        SimpleVibNormalModes refModes = MockSpecies.getSpecies0232VibrationalNormalModes();
        for (SimpleVibNormalMode refMode : refModes) {
            boolean identified = false;
            for (Vibration mode : modes) {
                if (refMode.equals(mode)) {
                    identified = true;
                }
            }
            assert (identified);
        }
    }

    protected IRCompChemWrapper getCompChemWrapper(File file) throws Exception {
        // read compchem
        CMLCompChemParser parser = new CMLCompChemParser();
        parser.set(file);
        parser.parse();

        // read data from compchem
        return new IRCompChemWrapper(parser.get());
    }
}
