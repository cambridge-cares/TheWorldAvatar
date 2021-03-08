package gigadot.chom.chem.reader;

import gigadot.chom.chem.property.Vibration;
import gigadot.chom.chem.structure.Bond;
import gigadot.chom.chem.structure.BondType;
import gigadot.chom.chem.structure.Compound;
import gigadot.chom.chem.structure.CompoundDelegator;
import gigadot.chom.chem.structure.tool.StructureTools;
import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.CompChemIOUtils;
import gigadot.chom.compchem.CompChemWrapper;
import gigadot.chom.compchem.orm.annotation.processor.CompChemAnnotationProcessor;
import gigadot.chom.compchem.property.PMOI;
import gigadot.chom.compchem.property.RotationalConstants;
import gigadot.chom.compchem.xml.NamespaceUtils;
import gigadot.chom.model.cookie.EnthalpyOfFormation;
import gigadot.chom.model.brownie.Atom;
import gigatools.extra.cmlxom.CMLException;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import nu.xom.Nodes;
import org.xmlcml.cml.base.CMLNamespace;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLAtomArray;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLBondArray;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLPropertyList;
import org.xmlcml.euclid.Point3;

/**
 *
 * @author Weerapong
 */
public class CompChemReader extends ChemFileReader {

    private Compound compound = null;
    CompChemWrapper ccw = null;
    private Map<String, Integer> atomMap = null;
    private String file = "";

    /**
     * Read from data from compchem to compound. if compound contains old compchem in compchem map then it will be
     * removed. We use a default extractor scheme implemented in CompChemWrapper.
     *
     * @param cc
     * @param compound
     */
    public void read(CompChem cc, Compound compound) {
        compound.clear();
        this.compound = compound;
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

        CompChemAnnotationProcessor<CompoundDelegator> ccap = new CompChemAnnotationProcessor<CompoundDelegator>(cc);
        ccap.readEntity(compound);
    }

    /**
     * Read and parse molecular quantum calculation from compchem CML and
     * return information in given JChemDocument object.
     *
     * @see #read(String)
     */
    @Override
    public void read(String file, Compound compound) throws IOException {
        try {
            CompChem cc = CompChemIOUtils.read(new File(file));
            read(cc, compound);
        } catch (CMLException ex) {
            throw new IOException(ex);
        }
    }

    private CMLProperty getProperty(String dictRef, String namespace) {
        String lname = NamespaceUtils.getLocalNameFromQName(dictRef);
        String prefix = ccw.getCompchem().getPrefixForNamespace(namespace);
        String ldictRef = prefix + ":" + lname;
        CMLPropertyList list = ccw.getPropertyList();
        if (list != null) {
            List<CMLProperty> props = list.getPropertyDescendantsByName(ldictRef);
            if (!props.isEmpty()) {
                return props.get(props.size() - 1);
            } else {
                return null;
            }
        } else {
            logger.warn("PropertyList not found.");
            return null;
        }
    }

    private void readAtomList() {
        CMLAtomArray atomArray = ccw.getFinalMolecule().getAtomArray();
        if (atomArray != null) {
            List<CMLAtom> atoms = atomArray.getAtoms();
            atomMap = new HashMap<String, Integer>(atoms.size());
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
                int A = atomMap.get(bonds.get(i).getAtomId(0)).intValue();
                int B = atomMap.get(bonds.get(i).getAtomId(1)).intValue();
                // in this version bond type is labelled by integer.
                Bond.createBond(compound.getAtomByOrder(A), compound.getAtomByOrder(B), BondType.parseCMLBondType(bonds.get(i).getOrder()), compound);
            }
        } else {
            logger.trace("BondArray not found for " + file + ". Bondings are generated.");
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
            logger.trace("cc:hf298.15K not found for " + file);
        }
    }

    private void readFrequencyData() {
        CMLProperty vibtable = getProperty(":vibrations", CompChem.COMPCHEM_NS);
        if (vibtable == null) {
            logger.trace("no frequencies for " + file);
            return;
        }
        String ccPrefix = ccw.getCompchem().getPrefixForNamespace(CompChem.COMPCHEM_NS);
        Nodes freqs = vibtable.query("//cml:array[@dictRef='" + ccPrefix + ":frequencies']", CMLNamespace.CML_XPATH);
        if (freqs.size() > 0) {
            CMLArray farray = (CMLArray) freqs.get(0);
            final double[] df = farray.getDoubles();
            for (int i = 0; i < df.length; i++) {
                Vibration vib = new Vibration();
                vib.Frequency = df[i];
                compound.addVibration(vib);
            }
        } else {
            //throw new RuntimeException("no frequencies");
            logger.trace("no frequencies in frequency table for " + file);
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
}
