package uk.ac.cam.ceb.como.chem.structure;

import uk.ac.cam.ceb.como.chem.property.Vibration;
import uk.ac.cam.ceb.como.chem.info.alias.ThermoAnalyzable;
import uk.ac.cam.ceb.como.chem.info.ThermoInfo;
import uk.ac.cam.ceb.como.chem.info.ThermoInfoImpl;
import uk.ac.cam.ceb.como.chem.info.VibrationInfo;
import uk.ac.cam.ceb.como.chem.info.VibrationInfoImpl;
import java.util.List;
import uk.ac.cam.ceb.como.chem.property.EnthalpyOfFormation;
import uk.ac.cam.ceb.como.chem.property.Vibrations;
import uk.ac.cam.ceb.como.chem.structure.util.BasicCompoundConverter;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.compchem.info.BasicInfo;
import uk.ac.cam.ceb.como.compchem.info.BasicInfoImpl;
import uk.ac.cam.ceb.como.compchem.info.ComputedInfo;
import uk.ac.cam.ceb.como.compchem.info.ComputedInfoImpl;
import uk.ac.cam.ceb.como.compchem.info.MolecularInfo;
import uk.ac.cam.ceb.como.compchem.info.MolecularInfoImpl;
import uk.ac.cam.ceb.como.compchem.orm.annotation.CompChemWrapperField;
import uk.ac.cam.ceb.como.compchem.orm.annotation.Parameter;
import uk.ac.cam.ceb.como.compchem.orm.annotation.Prefixes;
import uk.ac.cam.ceb.como.compchem.orm.annotation.Property;
import uk.ac.cam.ceb.como.compchem.property.PMOI;
import uk.ac.cam.ceb.como.compchem.property.RotationalConstants;

/**
 * The delegation pattern is used as workaround for the lack of multiple inheritance
 * in Java. A complex information is constructed from many class of information.
 *
 * All the methods in this class (except clear method) should be auto-generated using
 * IDE delegating function. You should not attempt to write them yourself.
 *
 * In the future, we will use proxy
 *
 * @author pb556
 */
@Prefixes({"cc : " + CompChem.COMPCHEM_NS, "g : " + CompChem.GAUSSIAN_NS, "nonSi : " + CompChem.NONSI_NS})
public abstract class CompoundDelegator implements Structure, BasicInfo, MolecularInfo, ComputedInfo, VibrationInfo, ThermoInfo, ThermoAnalyzable {

    @CompChemWrapperField
    private CompChemWrapper ccw;
    private Structure structure = new StructureImpl();
    private BasicInfo basicInfo = new BasicInfoImpl();
    private MolecularInfo molecularInfo = new MolecularInfoImpl();
    private ComputedInfo computedInfo = new ComputedInfoImpl();
    private ThermoInfo thermoInfo = new ThermoInfoImpl();
    private VibrationInfo vibrationInfo = new VibrationInfoImpl();

    @Override
    public void clear() {
        structure.clear();
        basicInfo.clear();
        molecularInfo.clear();
        computedInfo.clear();
        thermoInfo.clear();
        vibrationInfo.clear();
    }

    @Override
    @Property(dictRef = "cc:rotational_symmetry", units = "si:none")
    public void setRotationalSymmetryNumber(int rotationalSymmetryNumber) {
        molecularInfo.setRotationalSymmetryNumber(rotationalSymmetryNumber);
    }

    @Override
    public void setMultiplicity(int multiplicity) {
        molecularInfo.setMultiplicity(multiplicity);
    }

    @Override
    public void setCharge(int charge) {
        molecularInfo.setCharge(charge);
    }

    @Override
    public int getSpinMultiplicity() {
        return molecularInfo.getSpinMultiplicity();
    }

    @Override
    public int getRotationalSymmetryNumber() {
        return molecularInfo.getRotationalSymmetryNumber();
    }

    @Override
    public int getCharge() {
        return molecularInfo.getCharge();
    }

    @Override
    public void setMethod(String method) {
        basicInfo.setMethod(method);
    }

    @Override
    public void setJobType(String jobType) {
        basicInfo.setJobType(jobType);
    }

    @Override
    public void setInchi(String inchi) {
        basicInfo.setInchi(inchi);
    }

    @Override
    public void setEmpiricalFormula(String empiricalFormula) {
        basicInfo.setEmpiricalFormula(empiricalFormula);
    }

    @Override
    public void setBasis(String basis) {
        basicInfo.setBasis(basis);
    }

    @Override
    @Parameter(dictRef = "cc:method")
    public String getMethod() {
        return basicInfo.getMethod();
    }

    @Override
    @Parameter(dictRef = "g:goal")
    public String getJobType() {
        return basicInfo.getJobType();
    }

    @Override
    public String getInchi() {
        return basicInfo.getInchi();
    }

    @Override
    public String getEmpiricalFormula() {
        if (basicInfo.getEmpiricalFormula() == null || basicInfo.getEmpiricalFormula().isEmpty()) {
            basicInfo.setEmpiricalFormula(BasicCompoundConverter.convertToEmpiricalFormula(structure));
        }
        return basicInfo.getEmpiricalFormula();
    }

    @Override
    @Parameter(dictRef = "cc:basis")
    public String getBasis() {
        return basicInfo.getBasis();
    }

    @Override
    @Property(dictRef = "cc:scfenergy", units = "nonSi:hartree")
    public void setFinalSCFEnergyInHartree(double SCFEnergyInHartree) {
        computedInfo.setFinalSCFEnergyInHartree(SCFEnergyInHartree);
    }

    @Override
    public void setFinalSCFEnergy(double SCFEnergy) {
        computedInfo.setFinalSCFEnergy(SCFEnergy);
    }

    @Override
    public void setRotationalConstants(RotationalConstants rotConst) {
        computedInfo.setRotationalConstants(rotConst);
    }

    @Override
    public void setPricipleMOI(PMOI moi) {
        computedInfo.setPricipleMOI(moi);
    }

    @Override
    public RotationalConstants getRotationalConstants() {
        return computedInfo.getRotationalConstants();
    }

    @Override
    public PMOI getPrincipalMOI() {
        return computedInfo.getPrincipalMOI();
    }

    @Override
    public double getFinalSCFEnergyInHartree() {
        return computedInfo.getFinalSCFEnergyInHartree();
    }

    @Override
    public double getFinalSCFEnergy() {
        return computedInfo.getFinalSCFEnergy();
    }

    @Override
    public boolean removeBond(Bond b) {
        return structure.removeBond(b);
    }

    @Override
    public boolean hasBond(Bond b) {
        return structure.hasBond(b);
    }

    @Override
    public boolean hasAtom(Atom atom) {
        return structure.hasAtom(atom);
    }

    @Override
    public double getWeigth() {
        return structure.getWeigth();
    }

    @Override
    public List<Bond> clonedBondList() {
        return structure.clonedBondList();
    }

    @Override
    public double[] getCentreOfMassInA() {
        return structure.getCentreOfMassInA();
    }

    @Override
    public double[] getCentreOfMass() {
        return structure.getCentreOfMass();
    }

    @Override
    public int getBondCount() {
        return structure.getBondCount();
    }

    @Override
    public Bond getBond(int index) {
        return structure.getBond(index);
    }

    @Override
    public Atom getAtomByOrder(int atom_order) {
        return structure.getAtomByOrder(atom_order);
    }

    @Override
    public int getAtomCount() {
        return structure.getAtomCount();
    }

    @Override
    public Atom getAtom(int index) {
        return structure.getAtom(index);
    }

    @Override
    public Bond findSimilarBond(Bond b) {
        return structure.findSimilarBond(b);
    }

    @Override
    public Bond findBond(Bond b) {
        return structure.findBond(b);
    }

    @Override
    public Atom findAtom(Atom atom) {
        return structure.findAtom(atom);
    }

    @Override
    public void calculateBonds() {
        structure.calculateBonds();
    }

    @Override
    public Bond addBond(Bond bond) {
        return structure.addBond(bond);
    }

    @Override
    public Atom addAtom(Atom atom) {
        return structure.addAtom(atom);
    }

    @Override
    public void setHf(EnthalpyOfFormation hf) {
        thermoInfo.setHf(hf);
    }

    @Override
    public EnthalpyOfFormation getHf() {
        return thermoInfo.getHf();
    }

    @Override
    public void insertVibration(Vibration vibration, int i) {
        vibrationInfo.insertVibration(vibration, i);
    }

    @Override
    public int getVibrationCount() {
        return vibrationInfo.getVibrationCount();
    }

    @Override
    public Vibration getVibration(int index) {
        return vibrationInfo.getVibration(index);
    }

    @Override
    public void clearVibrations() {
        vibrationInfo.clearVibrations();
    }

    @Override
    public void addVibration(Vibration vibration) {
        vibrationInfo.addVibration(vibration);
    }
    
    @Override
    public void addVibrations(Vibrations vibrations) {
        vibrationInfo.addVibrations(vibrations);
    }
    
    @Override
    public Vibrations getVibrations() {
        return vibrationInfo.getVibrations();
    }
}
