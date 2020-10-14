package gigadot.chom.chem.structure;

import gigadot.chom.model.brownie.Atom;
import gigadot.chom.model.chemistry.Element;
import gigadot.chom.chem.property.Vibration;
import gigadot.chom.chem.info.alias.ThermoAnalyzable;
import gigadot.chom.chem.info.ThermoInfo;
import gigadot.chom.chem.info.ThermoInfoImpl;
import gigadot.chom.chem.info.VibrationInfo;
import gigadot.chom.chem.info.VibrationInfoImpl;
import gigadot.chom.chem.structure.tool.CompoundConverter;
import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.CompChemWrapper;
import gigadot.chom.compchem.info.BasicInfo;
import gigadot.chom.compchem.info.BasicInfoImpl;
import gigadot.chom.compchem.info.ComputedInfo;
import gigadot.chom.compchem.info.ComputedInfoImpl;
import gigadot.chom.compchem.info.MolecularInfo;
import gigadot.chom.compchem.info.MolecularInfoImpl;
import gigadot.chom.compchem.orm.annotation.CompChemWrapperField;
import gigadot.chom.compchem.orm.annotation.Parameter;
import gigadot.chom.compchem.orm.annotation.Prefixes;
import gigadot.chom.compchem.orm.annotation.Property;
import gigadot.chom.model.cookie.EnthalpyOfFormation;
import gigadot.chom.compchem.property.PMOI;
import gigadot.chom.compchem.property.RotationalConstants;
import java.util.List;

/**
 * The delegation pattern is used as workaround for the lack of multiple inheritance
 * in Java. A complex information is constructed from many class of information.
 *
 * All the methods in this class (except clear method) should be auto-generated using
 * IDE delegating function. You should not attempt to write them yourself.
 *
 * In the future, we will use proxy
 *
 * @author wp214
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
            basicInfo.setEmpiricalFormula(CompoundConverter.convertToEmpiricalFormula(structure));
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
}
