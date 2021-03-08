package uk.ac.cam.ceb.como.thermo.partition_function;

import uk.ac.cam.ceb.como.chem.info.alias.ThermoAnalyzable;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.function.simple.UniformFunction;

/**
 *
 * @author pb556
 */
public abstract class PartitionFunction {

    protected ThermoAnalyzable thermoAnalyzable = null;
    protected Function scalingFactor = null;

    public void setThermoAnalyzable(ThermoAnalyzable thermoAnalyzable) {
        this.thermoAnalyzable = thermoAnalyzable;
    }
    
    public void setScalingFactor(Function factor) {
        scalingFactor = factor;
    }
    
    public void setScalingFactor(double factor) {
        scalingFactor = new UniformFunction(factor);
    }
    
    public Function getScalingFactor() {
        return scalingFactor;
    }

    public abstract PartitionValues getPartitionValues(double T);
    
    public PartitionValues getPartitionValues(double T, double scalingFactor) {
        Function orig = getScalingFactor();
        setScalingFactor(scalingFactor);
        PartitionValues values = getPartitionValues(T);
        setScalingFactor(orig);
        return values;
    }
}
