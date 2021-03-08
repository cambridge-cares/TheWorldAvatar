package uk.ac.cam.ceb.como.chem.moi;

import uk.ac.cam.ceb.como.math.linear_algebra.VectorUtil;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import uk.ac.cam.ceb.como.compchem.property.PMOI;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.chem.structure.Compound;
import uk.ac.cam.ceb.como.chem.structure.Molecule;

/**
 *
 *
 * @author pb556
 */
public class PitzerMOICalculator {

    private Compound compound = null;
    private AxisType axis_type = AxisType.AXIS_OF_ROTOR_BOND;
    private double[] axis_direction = new double[3];
    private double[] ref_point = new double[3];

    public enum AxisType {

        AXIS_OF_ROTOR_BOND,
        AXIS_PASS_CENTRE_OF_MASS_OF_ROTATING_GROUP_PARALLEL_TO_ROTOR_BOND,
        AXIS_PASS_CENTRES_OF_MASS_OF_BOTH_GROUPS
    }

    public enum MOIApproxLevel {

        TOP_MOI,
        REDUCED_MOI,
        COUPLING_MOLECULAR_ROTATION_MOI,
        COUPLING_INTERNAL_ROTATIONS_MOI
    }

    public void setCompound(Compound compound) {
        this.compound = compound;
    }

    public void setAxisType(AxisType type) {
        axis_type = type;
    }

    /**
     * A generic get moment of inertia function.
     * @param level Level of approximation
     * @param bond a bond connecting top and the main molecule's body.
     * @param allRotorBonds cannot be null only if level is COUPLING_INTERNAL_ROTATIONS_MOI. For other level, it is not needed.
     * @return
     */
    
    public double getMOI(MOIApproxLevel level, Bond bond, Collection<Bond> allRotorBonds) {
        switch (level) {
            case TOP_MOI:
                return getTopMOI(bond);
            case REDUCED_MOI:
                return getReducedMOI(bond);
            case COUPLING_MOLECULAR_ROTATION_MOI:
                return getCouplingMolecularRotationMOI(bond);
            case COUPLING_INTERNAL_ROTATIONS_MOI:
                return getCouplingInternalRotationsMOI(bond, allRotorBonds);
        }
        return -1.0;
    }

    public double getTopMOI(Bond bond) {
        return getTopMOI(bond, true);
    }

    public double getReducedMOI(Bond bond) {
        List<Molecule> mols = createMoleculeListByRemovingBond(bond);
        if (mols.size() == 2) {
            setAxisProperties(bond, mols.get(0), mols.get(1));
            MOICalculator MOICal = new MOICalculator(mols.get(0));
            double I_R = MOICal.calculateMOI(axis_direction, ref_point);
            MOICal = new MOICalculator(mols.get(1));
            double I_L = MOICal.calculateMOI(axis_direction, ref_point);
            if (I_L + I_R == 0.0) {
                return -1.0;
            } else {
                return I_L * I_R / (I_L + I_R);
            }
        } else {
            return -1.0;
        }
    }

    public double getCouplingMolecularRotationMOI(Bond bond) {
        double Am = getTopMOI(bond, true);
        if (Am >= 0.0) {
            if (compound.getMoleculeCount() == 0) {
                compound.recreateMoleculeList();
            }
            if (compound.getMoleculeCount() == 1) {
                Molecule wmol = compound.getMolecule(0);
                PMOI pmoi = compound.getPrincipalMOI();
                double[] myBLamdaX = null;
                if (pmoi == null) {
                    MOICalculator MOICal = new MOICalculator(wmol);
                    pmoi = MOICal.calculatePrincipalMOI();
                    List<Molecule> mols = createMoleculeListByRemovingBond(bond);
                    setAxisProperties(bond, mols.get(0), mols.get(1));
                }
                myBLamdaX = getMyLamda(Am, pmoi, axis_direction);

                double biglamda = VectorUtil.getDotProduct(myBLamdaX, myBLamdaX);
                return Am - biglamda;
            }
        }
        return -1.0;
    }

    public double getCouplingInternalRotationsMOI(Bond bond, Collection<Bond> allRotorBonds) {
        // ensure that otherBonds doesn't contain bond.
        List<Bond> otherBonds = new ArrayList<Bond>(allRotorBonds);
        for (int i = 0; i < otherBonds.size(); i++) {
            if (otherBonds.get(i).equals(bond)) {
                otherBonds.remove(i);
                //break;
            }
        }
        if (compound.getMoleculeCount() == 0) {
            compound.recreateMoleculeList();
        }
        PMOI pmoi = compound.getPrincipalMOI();
        if (pmoi == null) {
            Molecule wmol = compound.getMolecule(0);
            MOICalculator MOICal = new MOICalculator(wmol);
            pmoi = MOICal.calculatePrincipalMOI();
        }
        // get I(3,n)_X
        double I3x = getCouplingMolecularRotationMOI(bond);
        // Axis has been set in getCouplingMolecularRotationMOI(bond);
        double Am_X = getTopMOI(bond, false);
        if (Am_X >= 0.0) {
            // get mBLamda_X = Am(lamda_x1/sqrt(I_1), lamda_x2/sqrt(I_2), lamda_x3/sqrt(I_3))
            // axis direction is set within the call of I3x = getCouplingMolecularRotationMOI(bond);
            // This can be used for axis_d in getMyLamda function.
            double[] mBLamda_X = getMyLamda(Am_X, pmoi, axis_direction);

            // loop to get all other I(3,n)_Y and mbLamda_Y = Am'(lamda_y1/sqrt(I_1), lamda_y2/sqrt(I_2), lamda_y3/sqrt(I_3))
            double[][] mbLamda_Y = new double[otherBonds.size()][];
            double[] I3y = new double[otherBonds.size()];
            for (int i = 0; i < otherBonds.size(); i++) {
                I3y[i] = getCouplingMolecularRotationMOI(otherBonds.get(i));
                // Axis has been set in getCouplingMolecularRotationMOI(otherBonds.get(i));
                double Am_Y = getTopMOI(otherBonds.get(i), false);
                if (Am_Y < 0.0) {
                    return -1.0;
                }
                mbLamda_Y[i] = getMyLamda(Am_Y, pmoi, axis_direction);
            }

            // loop to get K = 1/2 * sum_Y(LambdaXY^2 / I(3,n)_Y)
            // where LambdaXY = mBLamda_X dot mbLamdaY[i]
            double K = 0.0;
            for (int i = 0; i < I3y.length; i++) {
                double LambdaXY = VectorUtil.getDotProduct(mBLamda_X, mbLamda_Y[i]);
                K += LambdaXY * LambdaXY / I3y[i];
            }
            K /= 2.0;

            // return I(4,n)_X = I(3,n)_X - K
            return I3x - K;
        }

        return -1.0;
    }

    private List<Molecule> createMoleculeListByRemovingBond(Bond bond) {
        List<Bond> bondList = compound.clonedBondList();
        for (int i = 0; i < bondList.size(); i++) {
            if (bondList.get(i).equals(bond)) {
                bondList.remove(i);
            }
        }

        return compound.createMoleculeListByConnectivity(bondList);
    }

    private void setAxisProperties(Bond bond, Molecule mol1, Molecule mol2, double[] axis_direction, double[] ref_point) {
        if (axis_type == AxisType.AXIS_OF_ROTOR_BOND) {
            axis_direction[0] = bond.getAtomA().getX() - bond.getAtomB().getX();
            axis_direction[1] = bond.getAtomA().getY() - bond.getAtomB().getY();
            axis_direction[2] = bond.getAtomA().getZ() - bond.getAtomB().getZ();
            VectorUtil.normalise(axis_direction);
            ref_point[0] = bond.getAtomA().getX();
            ref_point[1] = bond.getAtomA().getY();
            ref_point[2] = bond.getAtomA().getZ();
        } else if (axis_type == AxisType.AXIS_PASS_CENTRES_OF_MASS_OF_BOTH_GROUPS) {
            double[] cm1 = mol1.getCentreOfMass();
            double[] cm2 = mol2.getCentreOfMass();
            VectorUtil.getSubtraction(cm1, cm2, axis_direction);
            VectorUtil.normalise(axis_direction);
            ref_point[0] = cm1[0];
            ref_point[1] = cm1[1];
            ref_point[2] = cm1[2];
        } else if (axis_type == AxisType.AXIS_PASS_CENTRE_OF_MASS_OF_ROTATING_GROUP_PARALLEL_TO_ROTOR_BOND) {
            axis_direction[0] = bond.getAtomA().getX() - bond.getAtomB().getX();
            axis_direction[1] = bond.getAtomA().getY() - bond.getAtomB().getY();
            axis_direction[2] = bond.getAtomA().getZ() - bond.getAtomB().getZ();
            VectorUtil.normalise(axis_direction);
            double[] cm;
            if (mol1.getWeigth() < mol2.getWeigth()) {
                cm = mol1.getCentreOfMass();
            } else {
                cm = mol2.getCentreOfMass();
            }
            ref_point[0] = cm[0];
            ref_point[1] = cm[1];
            ref_point[2] = cm[2];
        }
    }

    private void setAxisProperties(Bond bond, Molecule mol1, Molecule mol2) {
        setAxisProperties(bond, mol1, mol2, axis_direction, ref_point);
    }

    private double getTopMOI(Bond bond, boolean setAxisProperties) {
        List<Molecule> mols = createMoleculeListByRemovingBond(bond);
        if (mols.size() == 2) {
            Molecule mol1, mol2;
            if (mols.get(0).getWeigth() <= mols.get(1).getWeigth()) {
                mol1 = mols.get(0);
                mol2 = mols.get(1);
            } else {
                mol1 = mols.get(1);
                mol2 = mols.get(0);
            }
            double[] axis_d;
            double[] ref_p;
            if (setAxisProperties) {
                setAxisProperties(bond, mol1, mol2);
                axis_d = axis_direction;
                ref_p = ref_point;
            } else {
                axis_d = new double[3];
                ref_p = new double[3];
                setAxisProperties(bond, mol1, mol2, axis_d, ref_p);
            }
            MOICalculator moiCal = new MOICalculator(mol1);
            double Am = moiCal.calculateMOI(axis_d, ref_p);
            return Am;
        }
        return -1.0;
    }

    private double[] getMyLamda(double Itop, PMOI pmoi, double[] axis_d) {
        double[] myL = new double[3];
        myL[0] = Itop * VectorUtil.getDotProduct(axis_d, pmoi.getPrincipalAxis(0)) / Math.sqrt(pmoi.getMOI(0));
        myL[1] = Itop * VectorUtil.getDotProduct(axis_d, pmoi.getPrincipalAxis(1)) / Math.sqrt(pmoi.getMOI(1));
        myL[2] = Itop * VectorUtil.getDotProduct(axis_d, pmoi.getPrincipalAxis(2)) / Math.sqrt(pmoi.getMOI(2));
        return myL;
    }
}
