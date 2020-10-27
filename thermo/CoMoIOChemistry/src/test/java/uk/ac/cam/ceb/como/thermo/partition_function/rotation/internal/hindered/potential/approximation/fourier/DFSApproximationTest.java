/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.approximation.fourier;

import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.approximation.fourier.DFSApproximation;
import java.io.File;
import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.data.DataSpace;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.approximation.ApproximationException;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.DiscrRotation;
import org.junit.Ignore;
import org.junit.Test;
import uk.ac.cam.ceb.como.io.thermo.file.name.internal_rotations.hindered.HPFileNameInfoExtended;
import uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered.HPDirectory;
import uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered.HPDirectoryParser;
import uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered.g09.HPD09DirParser;

/**
 *
 * @author pb556
 */
public class DFSApproximationTest {

    @Test
    @Ignore
    public void RotorFittingTest() throws ApproximationException, Exception {        
        cmp("W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test01\\data\\",
                "W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test01\\a-discr_freq-hr-fine-m-refined-species-0890-radical-0-restricted.g09", 0.3 * 1000);
        cmp("W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test02\\data\\",
                "W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test02\\sp_species-2-1-1680-radical-0.g09", 0.1 * 1000);
        cmp("W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test03\\data\\",
                "W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test03\\a-discr-freq-hr-fine-m-refined-species-0890-radical-0-restricted.g09", 0.3 * 1000);
        cmp("W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test04\\data\\",
                "W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test04\\sp_species-0036-radical-1.g09", 0.1 * 1000);
    }
    
    public void cmp(String dir, String ref, double tolerance) throws Exception {
        // general directory information
        HPDirectory directory = new HPDirectory(new File(dir), new File(ref));
        // parser for the defined directory
        HPDirectoryParser dirReader = new HPD09DirParser();
        dirReader.set(new File(dir));
        dirReader.setReference(new File(ref));
        dirReader.setFileNameInterpreter(new HPFileNameInfoExtended());

        // create the Rotation object
        dirReader.parse();
        DiscrRotation readRotation = directory.getRotation();
        DFSApproximation approx = new DFSApproximation(readRotation, (int) (readRotation.getDataSpace().size() / 2.0) + 1);
        Function f = approx.approximate();
        
        DataSpace space = readRotation.getDataSpace();
        for (DataPoint dp : space) {
            System.out.println(dp.getCoordinate()[0] + ", " + (Double) dp.getValue() + ", " + ((DataPoint) f.f((Double[]) dp.getCoordinate())).getValue());
            assert(Math.abs((Double) dp.getValue() - (Double) ((DataPoint) f.f((Double[]) dp.getCoordinate())).getValue()) < tolerance);
        }
    }
}
