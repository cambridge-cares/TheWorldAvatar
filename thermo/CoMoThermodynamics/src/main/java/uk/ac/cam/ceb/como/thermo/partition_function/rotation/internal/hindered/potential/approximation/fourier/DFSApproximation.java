/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.approximation.fourier;

import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.data.DataSpace;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeries;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficientFittingException;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimit;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimits;
import uk.ac.cam.ceb.como.math.fourier.series.onedimensional.OneDimensionalFourierSeriesException;
import uk.ac.cam.ceb.como.math.fourier.series.onedimensional.classical.ClassicalOneDimensionalFourierSeries;
import uk.ac.cam.ceb.como.math.fourier.series.onedimensional.discrete.DiscreteClassicalOneDimensionalFourierSeriesCoefficientFitting;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.approximation.ApproximationException;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.approximation.TorsionalBarrierApproximation;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.DiscrRotation;

/**
 *
 * @author pb556
 */
public class DFSApproximation extends TorsionalBarrierApproximation<Double> {
    
    protected FourierSeries fourierSeries;
    
    public DFSApproximation(DiscrRotation rotation) throws Exception {
        super(rotation);
        initFourierSeries();
    }
    
    public DFSApproximation(DiscrRotation rotation, int order) throws Exception {
        super(rotation);
        initFourierSeries();
    }
    
    protected final void initFourierSeries() throws FourierSeriesCoefficientFittingException, OneDimensionalFourierSeriesException {
        FourierSeriesLimits lower = new FourierSeriesLimits();
        FourierSeriesLimits upper = new FourierSeriesLimits();
        lower.add(new FourierSeriesLimit(1, 0.0));
        upper.add(new FourierSeriesLimit(1, 2.0 * Math.PI));
        DiscreteClassicalOneDimensionalFourierSeriesCoefficientFitting fitting = 
                new DiscreteClassicalOneDimensionalFourierSeriesCoefficientFitting(
                getDataSpace(), 
                (int) rotation.getDataSpace().size() / 2 + 1, 
                lower, upper);
        fitting.fitFourierSeriesCoefficients();
        fourierSeries = new ClassicalOneDimensionalFourierSeries(fitting.getFourierSeriesParameters());
    }
    
    protected final DataSpace getDataSpace() {
        return rotation.getDataSpace();
    }

    @Override
    public Function approximate() throws ApproximationException {
        return fourierSeries;
    }

    @Override
    public DataPoint approximate(Double[] coordinate) throws ApproximationException, FunctionCalculationException {
        return fourierSeries.f(coordinate);
    }
}
