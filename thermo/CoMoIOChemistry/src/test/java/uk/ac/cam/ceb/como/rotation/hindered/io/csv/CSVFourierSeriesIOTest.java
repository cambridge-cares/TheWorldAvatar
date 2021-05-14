/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.rotation.hindered.io.csv;

import java.io.File;
import java.io.FileFilter;
import java.util.HashMap;
import java.util.Map;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeries;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.approximation.fourier.DFSApproximation;
import uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered.HPDirectory;
import uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered.HPDirectoryParser;
import org.junit.Ignore;
import org.junit.Test;
import uk.ac.cam.ceb.como.chem.property.Rotation;
import uk.ac.cam.ceb.como.io.chem.file.filter.extension.specific.G09FileFilter;
import uk.ac.cam.ceb.como.io.thermo.file.name.internal_rotations.hindered.HPFileNameInfoDefault;
import uk.ac.cam.ceb.como.io.thermo.file.name.internal_rotations.hindered.HPFileNameInfoExtended;
import uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered.csv.CSVFourierSeriesParser;
import uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered.csv.CSVFourierSeriesWriter;
import uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered.g09.HPD09DirParser;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.DiscrRotation;

/**
 *
 * @author pb556
 */

public class CSVFourierSeriesIOTest {

    @Test
    @Ignore
    public void writtingTest() throws Exception {
        // write the fourier coefficients
        
        String dest = "W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\fit.csv";
        
        Map<String, FourierSeries> series = new HashMap<String, FourierSeries>();
        series.put("sp_species-2-1-1680-radical-0.g09", (FourierSeries) fit("W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test02\\data\\",
                "W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test02\\sp_species-2-1-1680-radical-0.g09").approximate());
        series.put("sp_species-0036-radical-1.g09", (FourierSeries) fit("W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test04\\data\\",
                "W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test04\\sp_species-0036-radical-1.g09").approximate());
        series.put("a-discr_freq-hr-fine-m-refined-species-0890-radical-0-restricted.g09", (FourierSeries) fit("W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test01\\data\\",
                "W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test01\\a-discr_freq-hr-fine-m-refined-species-0890-radical-0-restricted.g09").approximate());
        series.put("a-discr-freq-hr-fine-m-refined-species-0890-radical-0-restricted.g09", (FourierSeries) fit("W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test03\\data\\",
                "W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\test03\\a-discr-freq-hr-fine-m-refined-species-0890-radical-0-restricted.g09").approximate());
        
        CSVFourierSeriesWriter writer = new CSVFourierSeriesWriter();
        writer.setContent(series);
        writer.set(dest);
        writer.write();
        
        CSVFourierSeriesParser parser = new CSVFourierSeriesParser();
        parser.set(dest);
        parser.parse();

        assert(cmp(series, (HashMap<String, FourierSeries>) parser.get()));
    }
    
    public boolean cmp(Map<String, FourierSeries> seriesA, Map<String, FourierSeries> seriesB) {
        for (String keyA : seriesA.keySet()) {
            if (!seriesB.containsKey(keyA)) {
                return false;
            }
            FourierSeries fsA = seriesA.get(keyA);
            FourierSeries fsB = seriesB.get(keyA);
            if (!fsA.equals(fsB)) {
                return false;
            }
        }
        return true;
    }

    public DFSApproximation fit(String dir, String ref) throws Exception {
        // general directory information
        HPDirectory directory = new HPDirectory(new File(dir), new File(ref));
        // parser for the defined directory
        HPDirectoryParser dirReader = new HPD09DirParser();   //(directory, false, new ExtendedHRFileNameInterpreter(), 1.0);
        dirReader.set(new File(dir));
        dirReader.set(new FileFilter[]{new G09FileFilter()});
        dirReader.setReference(new File(ref));
        dirReader.setFileNameInterpreter(new HPFileNameInfoExtended());
        
        dirReader.parse();
        DiscrRotation rotation = dirReader.get().getRotation();
        

        // create the Rotation object
//        dirReader.createRotationObject();
//        Rotation readRotation = directory.getRotation();
        return new DFSApproximation(rotation, (int) (rotation.getDataSpace().size() / 2.0) + 1);
    }
}