package uk.ac.cam.ceb.como.thermo.converter.nasa7.fitting;

import uk.ac.cam.ceb.como.thermo.converter.nasa7.NASA7;
import uk.ac.cam.ceb.como.thermo.property.ThermoState;
import uk.ac.cam.ceb.como.thermo.calculator.nasa7.NASA7ThermoCalculator;
import uk.ac.cam.ceb.como.thermo.calculator.ThermoCalculator;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.math.linear.Array2DRowRealMatrix;
import org.apache.commons.math.linear.LUDecompositionImpl;
import org.apache.commons.math.linear.RealMatrix;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class LargrangianNASA7PolyFitting {

    private Logger logger = Logger.getLogger(getClass().getName());
    private ThermoCalculator thermoCal = null;
    private int n_low = 1000;
    private int n_high = 1000;
    private double T_min = 200;
    private double T_mid = 1000;
    private double T_max = 4000;
    private boolean isThermoDataChanged = false;
    private NASA7 nasa7 = null;
    private List<ThermoState> thermoData_low = new ArrayList<ThermoState>();
    private List<ThermoState> thermoData_high = new ArrayList<ThermoState>();

    /**
     * Constructor of <code>NASA7PolyFitting</code> by giving the number of
     * fitting points in each temperature range.
     *
     * @param n_low Number of fitting point for low temperature range. Mid point
     * is included by default.
     * @param n_high Number of fitting point for high temperature range. Mid
     * point is included by default.
     */
    public LargrangianNASA7PolyFitting(int n_low, int n_high) {
        setFittingPoints(n_low, n_high);
    }

    /**
     * Default constructor of <code>NASA7PolyFitting</code>.
     */
    public LargrangianNASA7PolyFitting() {
    }

    /**
     * Set the number of fitting points in each temperature range.
     *
     * @param n_low Number of fitting point for low temperature range. Mid point
     * is included by default.
     * @param n_high Number of fitting point for high temperature range. Mid
     * point is included by default.
     */
    public final void setFittingPoints(int n_low, int n_high) {
        if ((n_low > 0) && (n_high > 0)) {
            this.n_low = n_low;
            this.n_high = n_high;
            isThermoDataChanged = true;
        } else {
            logger.warn("Number of NASA 7-coefficient polynomial fitting points is less than 1. Current values of " + n_low + " and " + n_high + " are used.");
        }
    }

    /**
     * Set themochemistry calculator to <code>NASA7PolyFitting</code>
     *
     * @param thermoCal <code>ThermoCalculator</code> object.
     */
    public void setThermoCalculator(ThermoCalculator thermoCal) {
        if (thermoCal != null) {
            this.thermoCal = thermoCal;
            isThermoDataChanged = true;
        }
    }

    /**
     * Set temperature bounds for NASA 7-coefficient polynomial fitting.
     *
     * @param T_min Set temperature lower bound.
     * @param T_mid Set temperature midpoint.
     * @param T_max Set temperature upper bound.
     * @throws RuntimeException ("Invalid temperature ranges.");
     */
    public void setTemperatureBounds(double T_min, double T_mid, double T_max) {
        if ((0 < T_min) && (T_min < T_mid) && (T_mid < T_max)) {
            this.T_min = T_min;
            this.T_mid = T_mid;
            this.T_max = T_max;
            isThermoDataChanged = true;
        } else {
            throw new RuntimeException("Invalid temperature ranges.");
        }
    }

    /**
     * Fit and get NASAPolynomial object.
     *
     * @return NASAPolynomial object.
     */
    public NASA7ThermoCalculator getNASA7ThermoCalculator() throws Exception {
        if (isThermoDataChanged) {
            reGenerateThermoData();
            reFitPolyToData();
        }
        NASA7ThermoCalculator nasa_poly = new NASA7ThermoCalculator(nasa7);
        return nasa_poly;
    }

    /**
     * Re-generate thermochemistry data for fitting.
     */
    private void reGenerateThermoData() {
        thermoData_low.clear();
        thermoData_high.clear();
        thermoCal.setPressure(PhysicalConstants.P_1atm);
        ThermoState thermo = null;
        double T_step_low = (T_mid - T_min) / n_low;
        logger.info("Fitting low temperature polynomials...");
        for (int i = 0; i < n_low; i++) {
            //logger.info("Iteration " + (i + 1) + " / " + n_low + " for the low temperature polynomials.");
            try {
                double T = (T_step_low * i) + T_min;
                thermo = thermoCal.getThermoState(T);
                thermoData_low.add(thermo);
            } catch (Exception e) {
                logger.error("Thermo state for " + ((T_step_low * i) + T_min) + "K could not be calculated...");
            }
        }

        double T_step_high = (T_max - T_mid) / n_high;
        logger.info("Fitting high temperature polynomials...");
        for (int i = 0; i < n_high; i++) {
            //logger.info("Iteration " + (i + 1) + " / " + n_high + " for the high temperature polynomials.");
            try {
                double T = (T_step_high * i) + T_mid;
                thermo = thermoCal.getThermoState(T);
                thermoData_high.add(thermo);
            } catch (Exception e) {
                logger.error("Thermo state for " + ((T_step_high * i) + T_mid) + "K could not be calculated...");
            }
        }
        try {
            thermo = thermoCal.getThermoState(T_max);
        } catch (Exception e) {
            logger.error("Thermo state for " + (T_max) + "K could not be calculated...");
        }
        thermoData_high.add(thermo);
    }

    /**
     * Fit the polynomials to generated thermochemistry data.
     */
    private void reFitPolyToData() throws Exception {
        // This line get first element at index 0. this might cuase error if there is
        // no element. might need to add checking.
        ThermoState thermoConst = thermoData_high.get(0);
        double[] poly_low = doFitting(thermoData_low, thermoConst);
        double[] poly_high = doFitting(thermoData_high, thermoConst);
        nasa7 = new NASA7();
        nasa7.setTemperatureLowerBound(T_min);
        nasa7.setTemperatureMidPoint(T_mid);
        nasa7.setTemperatureUpperBound(T_max);
        for (int i = 0; i < 5; i++) {
            nasa7.setALow(i, poly_low[i]);
            nasa7.setAHigh(i, poly_high[i]);
        }
        // Correct low temperature coeff
        ThermoState thermo = thermoCal.getThermoState(PhysicalConstants.T_25C);
        NASA7ThermoCalculator nasa7Cal = new NASA7ThermoCalculator(nasa7);
        ThermoState thermo_nasa = nasa7Cal.getThermoState(PhysicalConstants.T_25C);
        double a6 = (thermo.H - thermo_nasa.H) / PhysicalConstants.R;
        double a7 = (thermo.S - thermo_nasa.S) / PhysicalConstants.R;
        nasa7.setALow(5, a6);
        nasa7.setALow(6, a7);
        // Correct high temperature coeff
        thermo = thermoCal.getThermoState(T_mid);
        thermo_nasa = nasa7Cal.getThermoState(T_mid);
        a6 = (thermo.H - thermo_nasa.H) / PhysicalConstants.R;
        a7 = (thermo.S - thermo_nasa.S) / PhysicalConstants.R;
        nasa7.setAHigh(5, a6);
        nasa7.setAHigh(6, a7);
    }

    /**
     * Do fitting NASA first 5 of 7-coefficient individually to Cp in the
     * thermochemistry vector.
     *
     * @param thermoData Thermochemistry data include those at those at the
     * boundaries.
     * @return an array of double containing first 5 coefficients.
     */
    private double[] doFitting(List<ThermoState> thermoData, ThermoState thermoConst) {
        RealMatrix Cp = vec_Cp(thermoData);
        RealMatrix transDCp = met_transDCp(thermoData);
        RealMatrix DCp = transDCp.transpose();
        RealMatrix N = DCp.multiply(Cp).scalarMultiply(2.0);
        RealMatrix P = DCp.multiply(transDCp).scalarMultiply(2.0);
        RealMatrix cTb = vec_cTB(thermoConst);
        RealMatrix dCpTB = vec_dCpTB(thermoConst.T);
        RealMatrix inverseP = new LUDecompositionImpl(P).getSolver().getInverse();
        RealMatrix G = dCpTB.transpose().multiply(inverseP);
        RealMatrix Y = new LUDecompositionImpl(G.multiply(dCpTB)).getSolver().getInverse();
        RealMatrix Z = G.multiply(N);

        RealMatrix A = inverseP.multiply(N.subtract((dCpTB.multiply(Y).multiply(Z.subtract(cTb)))));

        double[] poly_a = new double[5];
        for (int i = 0; i < 5; i++) {
            poly_a[i] = A.getEntry(i, 0);
        }
        return poly_a;
    }

    /**
     * Cp vector.
     *
     * @param thermoData Thermochemistry data to be fited.
     * @return Cp vector.
     */
    private RealMatrix vec_Cp(List<ThermoState> thermoData) {
        RealMatrix vec_Cp = new Array2DRowRealMatrix(thermoData.size(), 1);
        for (int i = 0; i < thermoData.size(); i++) {
            vec_Cp.setEntry(i, 0, thermoData.get(i).Cp / PhysicalConstants.R);
        }
        return vec_Cp;
    }

    /**
     * Temperature matrix.      <code>
     * |1 T T<sup>2</sup> T<sup>3</sup> T<sup>4</sup>| |1 T T<sup>2</sup>
     * T<sup>3</sup> T<sup>4</sup>| |. . . . . . . | |. . . . . . . | |. . . . .
     * . . | |1 T T<sup>2</sup> T<sup>3</sup> T<sup>4</sup>| |1 T T<sup>2</sup>
     * T<sup>3</sup> T<sup>4</sup>|
     * </code>
     *
     * @param thermoData Thermochemistry data to be fited.
     * @return Temperature matrix.
     */
    private RealMatrix met_transDCp(List<ThermoState> thermoData) {
        RealMatrix met_T = new Array2DRowRealMatrix(thermoData.size(), 5);
        for (int i = 0; i < thermoData.size(); i++) {
            double T = thermoData.get(i).T;
            for (int j = 0; j < 5; j++) {
                met_T.setEntry(i, j, Math.pow(T, j));
            }
        }
        return met_T;
    }

    private RealMatrix vec_cTB(ThermoState thermoConst) {
        RealMatrix vec_cTB = new Array2DRowRealMatrix(1, 1);
        vec_cTB.setEntry(0, 0, thermoConst.Cp / PhysicalConstants.R);
        return vec_cTB;
    }

    private RealMatrix vec_dCpTB(double T) {
        RealMatrix vec_dCpTB = new Array2DRowRealMatrix(5, 1);
        //RealMatrix vec_dCpTB = new Matrix(5, 1);
        for (int i = 0; i < 5; i++) {
            vec_dCpTB.setEntry(i, 0, Math.pow(T, i));
        }
        return vec_dCpTB;
    }
}
/* Optimisation method using largrangian multiplier to constraint Cp to pass the T_mid.
 \[\begin{array}{l}
 {\min _{\bf{x}}}\{ f({\bf{a}})\} {\rm{ subject to }}{\bf{g}}({\bf{a}}) = {\bf{0}} \\
 f({\bf{a}}) = \sum\limits_i^n {{{\left( {{C_{p,{T_i}}} - {C_{p,{T_i}}}({\bf{a}})} \right)}^2}}  \\
 {\bf{g}}({\bf{a}}) = \left( {{C_{p,{T_B}}}({\bf{a}}) - {C_{p,{T_B}}}} \right)\rm{   ,     } \bf{\lambda } = \left( {{\lambda _{{C_{p,{T_B}}}}}} \right){\rm{     ,      }}{\bf{a}} = \left( {\begin{array}{*{20}{c}}
 a_1  \\
 a_2  \\
 a_3  \\
 a_4  \\
 a_5  \\
 \end{array}} \right) \\
 L({\bf{a}},{\bf{\lambda }}) = f({\bf{a}}) + {{\bf{\lambda }}^T}{\bf{g}}({\bf{a}}) \\
 {\nabla _{\bf{a}}}L({{\bf{a}}_\diamondsuit },{{\bf{\lambda }}_\diamondsuit }) = {\bf{0}}{\rm{ at local minimum}} \\
 {\nabla _{\bf{a}}}L({\bf{a}},{\bf{\lambda }}) = {\nabla _{\bf{a}}}f({\bf{a}}) + {\nabla _{\bf{a}}}\left( {{{\bf{\lambda }}^T}{\bf{g}}({\bf{a}})} \right) \\
 {C_{p,{T_i}}}({\bf{a}}) = \frac{{C_{p,{T_i}}^{Cal}}}{R} = \left( {\begin{array}{*{20}{c}}
 1 & {{T_i}} & {T_i^2} & {T_i^3} & {T_i^4}  \\
 \end{array}} \right){\bf{a}} = {\bf{d}}_{{C_{p,{T_i}}}}^T{\bf{a}} \\
 \delta {C_{p,{T_i}}}({\bf{a}}) = {C_{p,{T_i}}} - {\bf{d}}_{{C_{p,{T_i}}}}^T{\bf{a}} \\
 {{\bf{k}}_{{C_p}}} = {k_{{C_p},i}} = \delta {C_{p,{T_i}}}({\bf{a}}) \\
 \sum\limits_i^n {{{\left( {{C_{p,{T_i}}} - {C_{p,{T_i}}}({\bf{a}})} \right)}^2}}  = \sum\limits_i^n {{{\left( {{C_{p,{T_i}}} - {\bf{d}}_{{C_{p,{T_i}}}}^T{\bf{a}}} \right)}^2}}  = \sum\limits_i^n {\delta C_{p,{T_i}}^2({\bf{a}}) = } {\bf{k}}_{{C_p}}^T{{\bf{k}}_{{C_p}}} \\
 f({\bf{a}}) = {\bf{k}}_{{C_p}}^T{{\bf{k}}_{{C_p}}} \\
 {\nabla _{\bf{a}}}f({\bf{a}}) = {\nabla _{\bf{a}}}\left( {{\bf{k}}_{{C_p}}^T{{\bf{k}}_{{C_p}}}} \right) = 2{\bf{k}}_{{C_p}}^T{\nabla _{\bf{a}}}{{\bf{k}}_{{C_p}}} \\
 {\nabla _{\bf{a}}}{{\bf{k}}_{{C_p}}} = \frac{{\partial \left( {{C_{p,{T_i}}} - {\bf{d}}_{{C_{p,{T_i}}}}^T{\bf{a}}} \right)}}{{\partial {a_j}}} =  - \frac{{\partial \left( {{\bf{d}}_{{C_{p,{T_i}}}}^T{\bf{a}}} \right)}}{{\partial {a_j}}} =  - {\bf{d}}_{{C_{p,{T_i}}}}^T\frac{{\partial {\bf{a}}}}{{\partial {a_j}}} =  - {\bf{d}}_{{C_{p,{T_i}}}}^T\frac{{\partial {a_i}}}{{\partial {a_j}}} =  - {\bf{d}}_{{C_{p,{T_i}}}}^T{\delta _{ij}} =  - {\bf{d}}_{{C_{p,{T_i}}}}^T{\bf{I}} =  - {\bf{D}}_{{C_p}}^T \\
 {{\bf{D}}_{{C_p}}} = \left( {\begin{array}{*{20}{c}}
 {{{\bf{d}}_{{C_{p,{T_1}}}}}} & {{{\bf{d}}_{{C_{p,{T_2}}}}}} &  \ldots  & {{{\bf{d}}_{{C_{p,{T_n}}}}}}  \\
 \end{array}} \right) \\
 {\nabla _{\bf{a}}}f({\bf{a}}) =  - 2\left( {{\bf{k}}_{{C_p}}^T{\bf{D}}_{{C_p}}^T} \right) \\
 {\bf{g}}({\bf{a}}) = \left( {\begin{array}{*{20}{c}}
 {{\bf{d}}_{{C_{p,{T_B}}}}^T{\bf{a}} - {C_{p,{T_B}}}}  \\
 \end{array}} \right) = \left( {\begin{array}{*{20}{c}}
 {{\bf{d}}_{{C_{p,{T_B}}}}^T}  \\
 \end{array}} \right){\bf{a}} - \left( {\begin{array}{*{20}{c}}
 {{C_{p,{T_B}}}}  \\
 \end{array}} \right) = {\bf{M}}_{{T_B}}^T{\bf{a}} - {{\bf{c}}_{{T_B}}} \\
 {\nabla _{\bf{a}}}\left( {{{\bf{\lambda }}^T}{\bf{g}}({\bf{a}})} \right) = {{\bf{\lambda }}^T}{\nabla _{\bf{a}}}{\bf{g}}({\bf{a}}) = {{\bf{\lambda }}^T}{\nabla _{\bf{a}}}\left( {{\bf{M}}_{{T_B}}^T{\bf{a}} - {{\bf{c}}_{{T_B}}}} \right) = {{\bf{\lambda }}^T}{\bf{M}}_{{T_B}}^T{\nabla _{\bf{a}}}{\bf{a}} = {{\bf{\lambda }}^T}{\bf{M}}_{{T_B}}^T \\
 {\nabla _{\bf{a}}}L({\bf{a}},{\bf{\lambda }}) = {\nabla _{\bf{a}}}f({\bf{a}}) + {\nabla _{\bf{a}}}\left( {{{\bf{\lambda }}^T}{\bf{g}}({\bf{a}})} \right) =  - 2\left( {{\bf{k}}_{{C_p}}^T{\bf{D}}_{{C_p}}^T} \right) + {{\bf{\lambda }}^T}{\bf{M}}_{{T_B}}^T \\
 {\nabla _{\bf{a}}}L({{\bf{a}}_\diamondsuit },{{\bf{\lambda }}_\diamondsuit }) =  - 2\left( {{\bf{k}}_{{C_p},\diamondsuit }^T{\bf{D}}_{{C_p}}^T} \right) + {\bf{\lambda }}_\diamondsuit ^T{\bf{M}}_{{T_B}}^T = {\bf{0}} \\
 {\bf{0}} =  - 2\left( {{{\left( {{C_{p,{T_i}}} - {\bf{d}}_{{C_{p,{T_i}}}}^T{{\bf{a}}_\diamondsuit }} \right)}^T}{\bf{D}}_{{C_p}}^T} \right) + {\bf{\lambda }}_\diamondsuit ^T{\bf{M}}_{{T_B}}^T \\
 {\bf{0}} =  - 2{{\bf{D}}_{{C_p}}}\left( {{C_{p,{T_i}}} - {\bf{d}}_{{C_{p,{T_i}}}}^T{{\bf{a}}_\diamondsuit }} \right) + {{\bf{M}}_{{T_B}}}{{\bf{\lambda }}_\diamondsuit } \\
 {\bf{0}} =  - 2\left( {{{\bf{d}}_{{C_{p,{T_i}}}}}{C_{p,{T_i}}} - {{\bf{d}}_{{C_{p,{T_i}}}}}{\bf{d}}_{{C_{p,{T_i}}}}^T{{\bf{a}}_\diamondsuit }} \right) + {{\bf{M}}_{{T_B}}}{{\bf{\lambda }}_\diamondsuit } \\
 {\bf{0}} =  - 2\left( {{{\bf{D}}_{{C_p}}}{{\bf{C}}_p} - {{\bf{D}}_{{C_p}}}{\bf{D}}_{{C_p}}^T{{\bf{a}}_\diamondsuit }} \right) + {{\bf{M}}_{{T_B}}}{{\bf{\lambda }}_\diamondsuit } \\
 {\rm{Let  }}{\bf{P}} = 2{{\bf{D}}_{{C_p}}}{\bf{D}}_{{C_p}}^T{\rm{  }},{\rm{   }}{\bf{N}} = 2{{\bf{D}}_{{C_p}}}{{\bf{C}}_p} \\
 {\bf{0}} =  - {\bf{N}} + {\bf{P}}{{\bf{a}}_\diamondsuit } + {{\bf{M}}_{{T_B}}}{{\bf{\lambda }}_\diamondsuit } \\
 {\bf{P}}{{\bf{a}}_\diamondsuit } =  - {{\bf{M}}_{{T_B}}}{{\bf{\lambda }}_\diamondsuit } + {\bf{N}} \\
 {{\bf{a}}_\diamondsuit } = {{\bf{P}}^{ - 1}}\left( { - {{\bf{M}}_{{T_B}}}{{\bf{\lambda }}_\diamondsuit } + {\bf{N}}} \right) \\
 {\bf{g}}({\bf{a}}) = {\bf{0}} = {\bf{M}}_{{T_B}}^T{{\bf{a}}_\diamondsuit } - {{\bf{c}}_{{T_B}}} = {\bf{M}}_{{T_B}}^T\left( {{{\bf{P}}^{ - 1}}\left( { - {{\bf{M}}_{T_B}}{{\bf{\lambda }}_\diamondsuit } + {\bf{N}}} \right)} \right) - {{\bf{c}}_{{T_B}}} \\
 {{\bf{c}}_{{T_B}}} = {\bf{M}}_{{T_B}}^T\left( {{{\bf{P}}^{ - 1}}\left( { - {{\bf{M}}_{{T_B}}}{{\bf{\lambda }}_\diamondsuit } + {\bf{N}}} \right)} \right) \\
 {{\bf{c}}_{{T_B}}} =  - {\bf{M}}_{{T_B}}^T{{\bf{P}}^{ - 1}}{{\bf{M}}_{{T_B}}}{{\bf{\lambda }}_\diamondsuit } + {\bf{M}}_{{T_B}}^T{{\bf{P}}^{ - 1}}{\bf{N}} \\
 {\bf{M}}_{{T_B}}^T{{\bf{P}}^{ - 1}}{{\bf{M}}_{{T_B}}}{{\bf{\lambda }}_\diamondsuit } = {\bf{M}}_{{T_B}}^T{{\bf{P}}^{ - 1}}{\bf{N}} - {{\bf{c}}_{{T_B}}} \\
 {{\bf{\lambda }}_\diamondsuit } = {\left( {{\bf{M}}_{{T_B}}^T{{\bf{P}}^{ - 1}}{{\bf{M}}_{{T_B}}}} \right)^{ - 1}}\left( {{\bf{M}}_{{T_B}}^T{{\bf{P}}^{ - 1}}{\bf{N}} - {{\bf{c}}_{{T_B}}}} \right) \\
 {\bf{a}}_\diamondsuit  = {{\bf{P}}^{ - 1}}\left( { - {{\bf{M}}_{{T_B}}}{{\left( {{\bf{M}}_{{T_B}}^T{{\bf{P}}^{ - 1}}{{\bf{M}}_{{T_B}}}} \right)}^{ - 1}}\left( {{\bf{M}}_{{T_B}}^T{{\bf{P}}^{ - 1}}{\bf{N}} - {{\bf{c}}_{{T_B}}}} \right) + {\bf{N}}} \right) \\
 {\bf{a}}_\diamondsuit  = {{\bf{P}}^{ - 1}}\left( { - {{\bf{d}}_{{C_{p,{T_B}}}}}{{\left( {{\bf{d}}_{{C_{p,{T_B}}}}^T{{\bf{P}}^{ - 1}}{{\bf{d}}_{{C_{p,{T_B}}}}}} \right)}^{ - 1}}\left( {{\bf{d}}_{{C_{p,{T_B}}}}^T{{\bf{P}}^{ - 1}}{\bf{N}} - {{\bf{c}}_{{T_B}}}} \right) + {\bf{N}}} \right) \\
 {\bf{a}}_\diamondsuit  = {{\bf{P}}^{ - 1}}\left( { - {{\bf{d}}_{{C_{p,{T_B}}}}}{\bf{Y}}\left( {{\bf{Z}} - {{\bf{c}}_{{T_B}}}} \right) + {\bf{N}}} \right) \\
 \bf{G} = {\bf{d}}_{{C_{p,{T_B}}}}^T{{\bf{P}}^{ - 1}} \\
 \bf{Y} = {\left( {{\bf{G}}{{\bf{d}}_{{C_{p,{T_B}}}}}} \right)^{ - 1}} \\
 \bf{Z} = {\bf{d}}_{{C_{p,{T_B}}}}^T{{\bf{P}}^{ - 1}}{\bf{N}} \\
 \end{array}\]
 */
