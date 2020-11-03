package gigadot.chom.chem.moi;

import gigadot.chom.compchem.math.ArrayMathUtils;
import gigadot.chom.compchem.property.PMOI;
import org.apache.log4j.Logger;
import gigatools.lite.constant.PhysicalConstants;
import gigadot.chom.model.brownie.Atom;
import gigadot.chom.chem.structure.Structure;
import gigadot.chom.chem.structure.tool.StructureTools;
import gigatools.lite.math.MatrixUtil;
import gigatools.lite.math.VectorUtil;
import org.apache.commons.math.linear.Array2DRowRealMatrix;
import org.apache.commons.math.linear.EigenDecomposition;
import org.apache.commons.math.linear.EigenDecompositionImpl;
import org.apache.commons.math.linear.RealMatrix;
import org.apache.commons.math.util.MathUtils;

/**
 * This is a moment of inertia calculator. It has a problem when calculating a symmetric molecule.
 * There is no dectection of two eigen values are the same. When the first two eigen value are very
 * closed there is a numerical issue with this calculation. Eigen vectors might not be orthogonal.
 *
 * @author Weerapong Phadungsukanan
 */
public class MOICalculator {

    private Logger logger = Logger.getLogger(getClass());
    private final Structure structure;

    public MOICalculator(Structure structure) {
        this.structure = structure;
    }

    /**
     * Calculate the moment of inertia about an axis which is defined by
     * axis direction and the point of reference.
     *
     * @param axis_direction
     * @param ref_point
     * @return
     * @throws NullPointerException is thrown if a structure is null.
     */
    public double calculateMOI(double[] axis_direction, double[] ref_point) {
        double I = 0.0;
        for (int k = 0; k < structure.getAtomCount(); k++) {
            Atom atom = structure.getAtom(k);
            // displacement between atom(k) and ref point
            double[] b = {atom.getX() - ref_point[0], atom.getY() - ref_point[1], atom.getZ() - ref_point[2]};
            double[] c = new double[3];
            double[] d = new double[3];
            VectorUtil.getCrossProduct(b, axis_direction, c);
            // find vector d which is in the direction of raduis from axis
            VectorUtil.getCrossProduct(axis_direction, c, d);
            // only normalise if the magnitude is not zero. d is now a unit vector
            // interface the direction of r.
            if (VectorUtil.getMagnitude(d) != 0.0) {
                VectorUtil.normalise(d);
            }
            // may need to check d in case that magnitude is so tiny
            // projection of b on unit vector d is the raduis
            // if d is zero then radius is zero
            double r = VectorUtil.getDotProduct(d, b);
            I += atom.getElement().atomicWeight * r * r;
        }
        if (StructureTools.isMOIZero(I)) {
            I = 0;
        }
        return I * PhysicalConstants.amu;
    }

    public double calculateMOIIn_amu_rbohr2(double[] axis_direction, double[] ref_point_InA) {
        double[] ref_point = ArrayMathUtils.copyAndScale(ref_point_InA, PhysicalConstants.A);
        return calculateMOI(axis_direction, ref_point) / PhysicalConstants.AtomicMOI;
    }

    public PMOI calculatePrincipalMOI() {
        final double[][] momentMatrix = MatrixUtil.createZeroMatrix(3, 3);
        final double[] cm = structure.getCentreOfMass();
        // Cal calculate I(i,j)
        for (int k = 0; k < structure.getAtomCount(); k++) {
            Atom atom = structure.getAtom(k);
            //displacement betweeb atom k and cm
            double[] x = {atom.getX() - cm[0], atom.getY() - cm[1], atom.getZ() - cm[2]};

            double x2 = x[0] * x[0];
            double y2 = x[1] * x[1];
            double z2 = x[2] * x[2];

            double w = atom.getElement().atomicWeight;

            momentMatrix[0][0] += w * (y2 + z2);
            momentMatrix[1][1] += w * (x2 + z2);
            momentMatrix[2][2] += w * (x2 + y2);

            momentMatrix[0][1] -= w * x[0] * x[1];
            momentMatrix[0][2] -= w * x[0] * x[2];
            momentMatrix[1][2] -= w * x[1] * x[2];

        }
        // symmetric matrix;
        momentMatrix[1][0] = momentMatrix[0][1];
        momentMatrix[2][0] = momentMatrix[0][2];
        momentMatrix[2][1] = momentMatrix[1][2];
        RealMatrix mat = new Array2DRowRealMatrix(momentMatrix);
        mat = mat.scalarMultiply(PhysicalConstants.amu);
        EigenDecomposition jeigen = new EigenDecompositionImpl(mat, MathUtils.SAFE_MIN);
        double[] tem_eigenval = jeigen.getRealEigenvalues();

        if (tem_eigenval.length != 3) {
            logger.warn("Number of eigenvalues is not 3.");
            throw new RuntimeException("Number of eigenvalues is not 3.");
        }

        // eigenvector[i] is the eigenvector of eigenvalues[i];
        final double[][] pmoi_axes = new double[3][3];
        final double[] pmoi_vals = new double[3];
        int zero_index = -1;
        for (int i = 0; i < tem_eigenval.length; i++) {
            if (tem_eigenval[i] < StructureTools.MOITreshold) {
                pmoi_vals[i] = 0;
                zero_index = i;
            } else {
                pmoi_vals[i] = tem_eigenval[i];
            }
        }
        if (zero_index >= 0) {
            double I = -1.0;
            // If it is linear then the other two principal MOIs are equal. Even if
            // it is an atom this is still valid.
            I = (pmoi_vals[0] + pmoi_vals[1] + pmoi_vals[2]) / 2.0;
            for (int i = 0; i < pmoi_vals.length; i++) {
                if (zero_index != i) {
                    pmoi_vals[i] = I;
                }
            }
        }
        RealMatrix eigvs = jeigen.getV();
        for (int k = 0; k < pmoi_vals.length; k++) {
            for (int i = 0; i < 3; i++) {
                pmoi_axes[k][i] = eigvs.getEntry(i, k);
            }
            VectorUtil.normalise(pmoi_axes[k]);
        }

        {//testMOIUncertainty();
            double error = VectorUtil.getDotProduct(pmoi_axes[0], pmoi_axes[1]) + VectorUtil.getDotProduct(pmoi_axes[1], pmoi_axes[2]) + VectorUtil.getDotProduct(pmoi_axes[0], pmoi_axes[2]);
            if (error > 0.0000001) {
                logger.warn("Warning : MOICalculator is used to calculate the moment of inertia with error of : " + error);
            }
        }
        PMOI pmoi = new PMOI(pmoi_vals, pmoi_axes);
        return pmoi;
    }
}
