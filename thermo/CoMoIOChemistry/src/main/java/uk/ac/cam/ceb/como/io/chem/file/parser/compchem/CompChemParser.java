package uk.ac.cam.ceb.como.io.chem.file.parser.compchem;

import uk.ac.cam.ceb.como.chem.property.Vibration;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.chem.structure.BondType;
import uk.ac.cam.ceb.como.chem.structure.Compound;
import uk.ac.cam.ceb.como.chem.structure.CompoundDelegator;
import uk.ac.cam.ceb.como.chem.structure.util.StructureTools;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.compchem.orm.annotation.processor.CompChemAnnotationProcessor;
import uk.ac.cam.ceb.como.compchem.property.PMOI;
import uk.ac.cam.ceb.como.compchem.property.RotationalConstants;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import nu.xom.Nodes;
import org.apache.log4j.Logger;

import org.xmlcml.cml.base.CMLNamespace;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLAtomArray;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLBondArray;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.euclid.Point3;
import uk.ac.cam.ceb.como.chem.property.EnthalpyOfFormation;
import uk.ac.cam.ceb.como.chem.structure.Atom;
import uk.ac.cam.ceb.como.io.chem.file.parser.ChemFileParser;

/**
 *
 * @author pb556
 */

public class CompChemParser extends ChemFileParser<Compound> {

    protected CompChemWrapper ccw = null;
    protected Map<String, Integer> atomMap = null;
    protected Compound compound = null;
    protected Logger logger = Logger.getLogger(getClass());

    /**
     * Read from data from compchem to compound. if compound contains old compchem in compchem map then it will be
     * removed. We use a default extractor scheme implemented in CompChemWrapper.
     *
     * @param cc
     */
    public void parse(CompChem cc) {
        
        this.compound = new Compound();
        atomMap = null;

        this.ccw = new CompChemWrapper(cc);

        readAtomList();
        readBondList();
        compound.recreateMoleculeList();

        readEnthalpyOfFormation();
        readFrequencyData();
        readCharge();
        readSpinMultiplicity();
        calculatePricipalMOI();
        calculateRotationalConstants();

        CompChemAnnotationProcessor<CompoundDelegator> ccap = new CompChemAnnotationProcessor<>(cc);
        ccap.readEntity(compound);
    }

    private void readAtomList() {
        CMLAtomArray atomArray = ccw.getFinalMolecule().getAtomArray();
        if (atomArray != null) {
            List<CMLAtom> atoms = atomArray.getAtoms();
            atomMap = new HashMap<>(atoms.size());
            for (int i = 0; i < atoms.size(); i++) {
                String symbol = atoms.get(i).getElementType();
                Atom atom = new Atom(symbol);
                compound.addAtom(atom);
                atomMap.put(atoms.get(i).getId(), i);
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
        CMLBondArray bondArray = ccw.getFinalMolecule().getBondArray();
        if (bondArray != null) {
            List<CMLBond> bonds = bondArray.getBonds();
            for (int i = 0; i < bonds.size(); i++) {
                int A = atomMap.get(bonds.get(i).getAtomId(0));
                int B = atomMap.get(bonds.get(i).getAtomId(1));
                // in this version bond type is labelled by integer.
                Bond.createBond(compound.getAtomByOrder(A), compound.getAtomByOrder(B), BondType.parseCMLBondType(bonds.get(i).getOrder()), compound);
            }
        } else {
            logger.trace("BondArray not found for " + f + ". Bondings are generated.");
            compound.calculateBonds();
        }
    }

    private void readEnthalpyOfFormation() {
        CMLProperty hf298 = getProperty(":hf298.15K", CompChem.COMPCHEM_NS);
        if (hf298 != null) {
            // Add Enthalpy of formation
            // T = PhysicalConstants.T_25C);
            double DHfInkcal_Per_mol = hf298.getScalarElements().get(0).getDouble();
            compound.setHf(EnthalpyOfFormation.from_kcal_Per_mol(DHfInkcal_Per_mol));
        } else {
            logger.trace("cc:hf298.15K not found for " + f);
        }
    }

    private void readFrequencyData() {
        CMLProperty vibtable = getProperty(":vibrations", CompChem.COMPCHEM_NS);
        if (vibtable == null) {
            logger.trace("no frequencies for " + f);
            return;
        }
        String ccPrefix = ccw.getCompchem().getPrefixForNamespace(CompChem.COMPCHEM_NS);
        Nodes freqs = vibtable.query("//cml:array[@dictRef='" + ccPrefix + ":frequencies']", CMLNamespace.CML_XPATH);
        if (freqs.size() > 0) {
            CMLArray farray = (CMLArray) freqs.get(0);
            final double[] df = farray.getDoubles();
            for (int i = 0; i < df.length; i++) {
                Vibration vib = new Vibration();
                vib.setFrequency(df[i]);
                compound.addVibration(vib);
            }
        } else {
            //throw new RuntimeException("no frequencies");
            logger.trace("no frequencies in frequency table for " + f);
        }
    }

    /**
     * Just calculate it. don't read from file.
     */
    private void calculatePricipalMOI() {
        compound.setPricipleMOI(StructureTools.calculatePrincipalMOI(compound.getMolecule(0)));
    }

    /**
     * Just calculate it
     */
    private void calculateRotationalConstants() {
        PMOI pmoi = compound.getPrincipalMOI();
        RotationalConstants rotConst = RotationalConstants.fromPMOI(new double[]{pmoi.getMOI(0), pmoi.getMOI(1), pmoi.getMOI(2)});
        compound.setRotationalConstants(rotConst);
    }

    private void readCharge() {
        try {
            compound.setCharge(ccw.getFinalMolecule().getFormalCharge());
        } catch (RuntimeException ex) {
            compound.setCharge(0);
            logger.warn("Charge not found in cml. Formal charge 0 is set.");
        }
    }

    private void readSpinMultiplicity() {
        try {
            compound.setMultiplicity(ccw.getFinalMolecule().getSpinMultiplicity());
        } catch (RuntimeException ex) {
            compound.setMultiplicity(1);
            logger.warn("Spin multiplicity not found in cml. A singlet state is set to molecule.");
        }
    }
    
    public CMLProperty getProperty(String dictRef, String namespace) {
        CompChemPropertyParser parser = new CompChemPropertyParser();
        parser.setCompChem(ccw.getCompchem());
        return parser.getProperty(dictRef, namespace);
    }

    @Override
    public void clear() throws Exception {
        compound.clear();
        f = null;
    }

    @Override
    public void parse() throws Exception {
        try {
            CompChem cc = CompChemIOUtils.read(f);
            parse(cc);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    public Compound get() throws Exception {
        return compound;
    }
}
