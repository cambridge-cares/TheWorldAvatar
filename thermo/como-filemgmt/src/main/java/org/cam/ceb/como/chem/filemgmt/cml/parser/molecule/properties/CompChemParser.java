/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.cml.parser.molecule.properties;

import gigadot.chom.chem.property.Vibration;
import gigadot.chom.chem.structure.Bond;
import gigadot.chom.chem.structure.BondType;
import gigadot.chom.chem.structure.Compound;
import gigadot.chom.chem.structure.CompoundDelegator;
import gigadot.chom.chem.structure.tool.StructureTools;
import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.CompChemWrapper;
import gigadot.chom.compchem.orm.annotation.processor.CompChemAnnotationProcessor;
import gigadot.chom.compchem.property.PMOI;
import gigadot.chom.compchem.property.RotationalConstants;
import gigadot.chom.model.brownie.Atom;
import gigadot.chom.model.cookie.EnthalpyOfFormation;
import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import nu.xom.Nodes;
import org.apache.log4j.Logger;
import org.cam.ceb.como.chem.filemgmt.parser.ChemFileParser;
import org.xmlcml.cml.base.CMLNamespace;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLAtomArray;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLBondArray;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.euclid.Point3;

/**
 *
 * @author pb556
 */
public class CompChemParser extends ChemFileParser{

    protected CompChemWrapper ccw = null;
    protected Compound comp = null;
    protected CompChem cc = null;
    private Map<String, Integer> atomMap = null;
    protected Logger logger = Logger.getLogger(getClass());

    public void setCompChem(CompChem cc) {
        this.cc = cc;
    }
    
    @Override
    public void set(File file) throws Exception {
        set(file.getAbsolutePath());
    }
    
    @Override
    public void set(String path) throws Exception {
        this.path = path;
        CMLCompChemParser parser = new CMLCompChemParser();
        parser.set(path);
        parser.parse();
        cc = parser.getCompChem();
    }
    
    @Override
    public Compound get() {
        return this.comp;
    }

    @Override
    public void parse() throws Exception {
        if (this.cc == null) {
            throw new Exception("CompChem object could not be created.");
        }

        this.ccw = new CompChemWrapper(this.cc);

        this.comp = new Compound();
        this.readAtomList();
        this.readBondList();
        ((Compound) this.comp).recreateMoleculeList();

        this.readEnthalpyOfFormation();
        this.readFrequencyData();
        this.readCharge();
        this.readSpinMultiplicity();
        calculatePricipalMOI();
        calculateRotationalConstants();

        CompChemAnnotationProcessor<CompoundDelegator> ccap = new CompChemAnnotationProcessor<CompoundDelegator>(this.ccw.getCompchem());
        ccap.readEntity((Compound) this.comp);
    }

    private void readAtomList() {
        CMLAtomArray atomArray = this.ccw.getFinalMolecule().getAtomArray();
        if (atomArray != null) {
            List<CMLAtom> atoms = atomArray.getAtoms();
            atomMap = new HashMap<String, Integer>(atoms.size());
            for (int i = 0; i < atoms.size(); i++) {
                String symbol = atoms.get(i).getElementType();
                Atom atom = new Atom(symbol);
                ((Compound) this.comp).addAtom(atom);
                atomMap.put(atoms.get(i).getId(), i);
                atom.setId(atoms.get(i).getId());
                atom.setOrder(i);
                Point3 point = atoms.get(i).getXYZ3();
                double posX = point.elementAt(0);
                double posY = point.elementAt(1);
                double posZ = point.elementAt(2);
                atom.setCoordinateInA(posX, posY, posZ);
            }
        } else {
            throw new RuntimeException("Molecule contains no atom");
        }
    }

    private void readBondList() {
        CMLBondArray bondArray = this.ccw.getFinalMolecule().getBondArray();
        if (bondArray != null) {
            List<CMLBond> bonds = bondArray.getBonds();
            for (int i = 0; i < bonds.size(); i++) {
                int A = atomMap.get(bonds.get(i).getAtomId(0)).intValue();
                int B = atomMap.get(bonds.get(i).getAtomId(1)).intValue();
                // in this version bond type is labelled by integer.
                Bond.createBond(((Compound) this.comp).getAtomByOrder(A), ((Compound) this.comp).getAtomByOrder(B), BondType.parseCMLBondType(bonds.get(i).getOrder()), ((Compound) this.comp));
            }
        } else {
            logger.trace("BondArray not found. Bondings are generated.");
            ((Compound) this.comp).calculateBonds();
        }
    }

    private void readEnthalpyOfFormation() {
        CMLProperty hf298 = getProperty(":hf298.15K", CompChem.COMPCHEM_NS);
        if (hf298 != null) {
            // Add Enthalpy of formation
            // T = PhysicalConstants.T_25C);
            double DHfInkcal_Per_mol = hf298.getScalarElements().get(0).getDouble();
            ((Compound) this.comp).setHf(EnthalpyOfFormation.from_kcal_Per_mol(DHfInkcal_Per_mol));
        } else {
            logger.trace("this.cc:hf298.15K not found.");
        }
    }

    private void readFrequencyData() {
        CMLProperty vibtable = getProperty(":vibrations", CompChem.COMPCHEM_NS);
        if (vibtable == null) {
            logger.trace("no frequencies");
            return;
        }
        String ccPrefix = this.ccw.getCompchem().getPrefixForNamespace(CompChem.COMPCHEM_NS);
        Nodes freqs = vibtable.query("//cml:array[@dictRef='" + ccPrefix + ":frequencies']", CMLNamespace.CML_XPATH);
        if (freqs.size() > 0) {
            CMLArray farray = (CMLArray) freqs.get(0);
            final double[] df = farray.getDoubles();
            for (int i = 0; i < df.length; i++) {
                Vibration vib = new Vibration();
                vib.Frequency = df[i];
                ((Compound) this.comp).addVibration(vib);
            }
        } else {
            //throw new RuntimeException("no frequencies");
            logger.trace("no frequencies in frequency table.");
        }
    }

    /**
     * Just calculate it. don't read from file.
     */
    private void calculatePricipalMOI() {
        this.comp.setPricipleMOI(StructureTools.calculatePrincipalMOI(this.comp.getMolecule(0)));
    }

    /**
     * Just calculate it
     */
    private void calculateRotationalConstants() {
        PMOI pmoi = this.comp.getPrincipalMOI();
        RotationalConstants rotConst = RotationalConstants.fromPMOI(new double[]{pmoi.getMOI(0), pmoi.getMOI(1), pmoi.getMOI(2)});
        this.comp.setRotationalConstants(rotConst);
    }

    private void readCharge() {
        try {
            ((Compound) this.comp).setCharge(this.ccw.getFinalMolecule().getFormalCharge());
        } catch (RuntimeException ex) {
            ((Compound) this.comp).setCharge(0);
            logger.warn("Charge not found in cml. Formal charge 0 is set.");
        }
    }

    private void readSpinMultiplicity() {
        try {
            ((Compound) this.comp).setMultiplicity(this.ccw.getFinalMolecule().getSpinMultiplicity());
        } catch (RuntimeException ex) {
            ((Compound) this.comp).setMultiplicity(1);
            logger.warn("Spin multiplicity not found in cml. A singlet state is set to molecule.");
        }
    }

    public CMLProperty getProperty(String dictRef, String namespace) {
        CompChemPropertyParser parser = new CompChemPropertyParser();
        parser.setCompChem(this.cc);
        return parser.getProperty(dictRef, namespace);
    }
}
