package uk.ac.cam.ceb.como.math.linear_algebra;

/**
 * 
 *
 * @author pb556
 */
public class MatrixUtil {

    public static void debugPrint(double[][] M) {
        for (int i = 0; i < M.length; i++) {
            System.out.print(M[i][0] + "   ");
            System.out.print(M[i][1] + "   ");
            System.out.println(M[i][2]);
        }
    }

    /**
     * Calculate rotational matrix about a given axis by a given angle. There is no check if
     * a rotationlAxis is 3D vector and rotMatrix is 3x3 matrix for fast calculation purpose.
     * You must make sure to have the right dimension before using this function.
     *
     * @param rotationalAxis define a rotational axis as an array of 3 double.
     * @param angle define the rotational angle in radian about the rotational axis.
     * @param rotMatrix return rotation matrix.
     */
    public static void getRotationMatrix(double[] rotationalAxis, double angle, double[][] rotMatrix) {
        double normAxis = VectorUtil.getMagnitude(rotationalAxis);

        double c = Math.cos(angle);
        double s = Math.sin(angle);
        double oneMinusC = 1 - c;

        double lx = rotationalAxis[0] / normAxis;
        double ly = rotationalAxis[1] / normAxis;
        double lz = rotationalAxis[2] / normAxis;

        double lxMultipyOneMinusC = lx * oneMinusC;
        double lyMultipyOneMinusC = ly * oneMinusC;
        double lzMultipyOneMinusC = lz * oneMinusC;

        rotMatrix[0][0] = lx * lxMultipyOneMinusC + c;
        rotMatrix[0][1] = ly * lxMultipyOneMinusC - lz * s;
        rotMatrix[0][2] = lz * lxMultipyOneMinusC + ly * s;

        rotMatrix[1][0] = lx * lyMultipyOneMinusC + lz * s;
        rotMatrix[1][1] = ly * lyMultipyOneMinusC + c;
        rotMatrix[1][2] = lz * lyMultipyOneMinusC - lx * s;

        rotMatrix[2][0] = lx * lzMultipyOneMinusC - ly * s;
        rotMatrix[2][1] = ly * lzMultipyOneMinusC + lx * s;
        rotMatrix[2][2] = lz * lzMultipyOneMinusC + c;
    }

    public static double[][] createZeroMatrix(int row, int col) {
        double[][] matrix = new double[row][col];
        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix[i].length; j++) {
                matrix[i][j] = 0.0;
            }
        }
        return matrix;
    }
}
