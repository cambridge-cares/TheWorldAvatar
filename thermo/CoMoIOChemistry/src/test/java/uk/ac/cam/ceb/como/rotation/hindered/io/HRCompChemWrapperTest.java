/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.rotation.hindered.io;

import java.io.File;
import java.util.List;
import org.junit.Test;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.io.chem.file.parser.cml.CMLCompChemParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.cml.CMLMoleculeParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParserInclDisplacements;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleAtom;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleBond;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleSpecies;

/**
 *
 * @author pb556
 */
public class HRCompChemWrapperTest {
    
    @Test
    public void getInitMoleculeTest_0232G09() throws Exception {
        String path = "test_data/ref/freq-hr-fine-species-0232-radical-0-restricted.g09";

        // read compchem
        FrequencyParserInclDisplacements parser = new FrequencyParserInclDisplacements();
        parser.set(new File(path));
        parser.parse();
        
        // read data from compchem
        IRCompChemWrapper hrCCW = new IRCompChemWrapper((CompChem) parser.get());

        // verify against read data
        assert (verify(hrCCW.getInitialMolecule(), MockSpecies.getSpecies0232(), 0, 1));
    }
    
    @Test
    public void getInitMoleculeTest_0232CMLDiff() throws Exception {
        String path = "test_data/cml/freq-hr-fine-species-0232-radical-0-restricted_diff_added.cml";
        
        // read compchem
        CMLCompChemParser parser = new CMLCompChemParser();
        parser.set(new File(path));
        parser.parse();
        
        // read data from compchem
        IRCompChemWrapper hrCCW = new IRCompChemWrapper(parser.get());

        // verify against read data
        assert (verify(hrCCW.getInitialMolecule(), MockSpecies.getSpecies0232Modified(), -1, 2));
    }
    
    @Test
    public void getFinalMoleculeTest_0232CMLDiff() throws Exception {
        String path = "test_data/cml/freq-hr-fine-species-0232-radical-0-restricted_diff_added.cml";
        
        // read compchem
        CMLCompChemParser parser = new CMLCompChemParser();
        parser.set(new File(path));
        parser.parse();
        
        // read data from compchem
        IRCompChemWrapper hrCCW = new IRCompChemWrapper(parser.get());
        // verify against read data
        assert (verify(hrCCW.getFinalMolecule(), MockSpecies.getSpecies0232(), 0, 1));
    }
    
    @Test
    public void getInitMoleculeTest_0232CML() throws Exception {
        String path = "test_data/cml/freq-hr-fine-species-0232-radical-0-restricted.cml";
        
        // read compchem
        CMLCompChemParser parser = new CMLCompChemParser();
        parser.set(new File(path));
        parser.parse();
        
        // read data from compchem
        IRCompChemWrapper hrCCW = new IRCompChemWrapper(parser.get());

        // verify against read data
        assert (verify(hrCCW.getFinalMolecule(), MockSpecies.getSpecies0232(), 0, 1));
    }
    
    @Test
    public void getFinalMoleculeTest_0232CML() throws Exception {
        String path = "test_data/cml/freq-hr-fine-species-0232-radical-0-restricted.cml";
        
        // read compchem
        CMLCompChemParser parser = new CMLCompChemParser();
        parser.set(new File(path));
        parser.parse();
        
        // read data from compchem
        IRCompChemWrapper hrCCW = new IRCompChemWrapper(parser.get());
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
    public void getInitParameterListTest() {
        
    }
    
    @Test
    public void getRotationalModesTest() {
             
    }
    
    @Test
    public void getRotationalModeTest() {
        
    }
    
    @Test
    public void getVibrationalModesTest() {
        
    }
}
