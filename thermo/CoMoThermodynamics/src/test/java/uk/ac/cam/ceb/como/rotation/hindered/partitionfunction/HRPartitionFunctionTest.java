/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.rotation.hindered.partitionfunction;

import uk.ac.cam.ceb.como.math.conversion.AngleConversion;
import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.data.DataSpace;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeries;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficientFittingException;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimit;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimits;
import uk.ac.cam.ceb.como.math.fourier.series.onedimensional.OneDimensionalFourierSeriesException;
import uk.ac.cam.ceb.como.math.fourier.series.onedimensional.discrete.DiscreteClassicalOneDimensionalFourierSeries;
import uk.ac.cam.ceb.como.math.fourier.series.onedimensional.discrete.DiscreteClassicalOneDimensionalFourierSeriesCoefficientFitting;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.DiscrRotation;
//import uk.ac.cam.ceb.como.thermo.partitionfunc.hr.HRSymmPartitionFunction;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class HRPartitionFunctionTest {

    @Test
    public void hrCH3PFTest() throws FourierSeriesCoefficientFittingException, OneDimensionalFourierSeriesException, Exception {
        // compare PF result with the more advanced one

        // take the cos function for the defined ch3 rotor

        // get max value
        // function v/2 * (1 - cos(n * alpha))

        // calculate fourier series

//        double T = 100.0;
//        double V = 8.0;
//        double Ir = 0.0;
//        int rotor_symmetry = 3;
//
//        DataSpace space = null;
//        Rotation rotation = new Rotation(null, space, null);
//
//        DFSApproximation approx = new DFSApproximation(rotation, (int) (rotation.getDataSpace().size() / 2.0));
//
//        HRPartitionFunction hrPF = new HRPartitionFunction(approx);
//        hrPF.setThermoAnalyzable(null);
//        hrPF.setReducedMoment(Ir);
//        hrPF.setRotorSymmetry(rotor_symmetry);
//        hrPF.getPartitionValues(T);
//
//        HRSymmPartitionFunction symmPF = new HRSymmPartitionFunction();
//        symmPF.setThermoAnalyzable(null);
//        symmPF.setBarrierEnergy(V);
//        symmPF.setReducedMoment(Ir, rotor_symmetry);
//        symmPF.getPartitionValues(T);


    }

    protected DataSpace getDataSpace(double v) throws FunctionCalculationException {
        Function f = new CH3RotorFunction(v);
        DataSpace space = new DataSpace();
        for (int i = 0; i <= 360; i += 10) {
            space.add(new DataPoint(new Double[]{AngleConversion.toRadians(i)}, f.f(i)));
        }
        return space;
    }

    protected class CH3RotorFunction extends Function<Double, Double> {

        private double v = 0.0;

        public CH3RotorFunction(double v) {
            this.v = v;
        }

        @Override
        public Double f(Double x, Object... additionalData) throws FunctionCalculationException {
            return (v / 2.0) * (1 - Math.cos(3 * x));
        }
    }
}
