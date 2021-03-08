
/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package uk.ac.cam.ceb.como.thermo.rotation.internal.validation;

/**
 *
 * @author pb556
 */

public class AngleDependentRedMOI {

//    @Test
//    public void readRotationalData() throws Exception {
//        String dirPath = "W:\\Data\\TTIP\\g09_hr\\test_run_angle_dep_moi\\[CH2]\\[CH2]_test7\\results\\";
//        String refPath = "W:\\Data\\TTIP\\g09_hr\\test_run_angle_dep_moi\\CH2_test\\freq-species-1522-2-1521.g09";
//        String topDescr = "[CH3]";
//        
//        Map<Double, Double> d = extractData(new G09FileFilter(new File(dirPath)).getValidFiles());
//
//        TorsionalBondIdentifier identifier = new TorsionalBondIdentifier(new CanonicalSMILESBuilder());
//
//        // simply read the files and extract the energies for the associated angles
//
//
////        HRDirectory dir = new HRDirectory(new File(dirPath), new File(refPath));
////        HRDirectoryReader reader = new G09DirectoryReader(dir, new DefaultHRFileNameInterpreter());
////        HashMap<Double, Double> angleAndIred = new HashMap<Double, Double>();
////        PitzerMOICalculator moiCalc = new PitzerMOICalculator();
////        if (reader.createRotationObject()) {
////            Rotation rot = reader.getDirectory().getRotation();
////            DataSpace data = rot.getDataSpace();
////            for (int i = 0; i < data.size(); i++) {
////                DataPoint p = data.get(i);
////                RotationalStep step = (RotationalStep) p.getValue();
////                double angle = step.getAngle();
////                CMLMolecule mol = step.getCMLMolecule();
////                Compound compound = CompoundConverter.convert(mol);
////                try {
////                    Collection<CMLBond> cmlBonds = identifier.identify(mol, topDescr);
////                    // how do i get this information? - minimum distance again
////                    for (CMLBond b : cmlBonds) {
////                        Bond torsBond = null;
////                        moiCalc.setCompound(compound);
////                        double Ired = moiCalc.getReducedMOI(CompoundConverter.convert(mol, compound, b));
////                        System.out.println(angle + ", " + Ired);
////                    }
////                } catch (Exception e) {
////                }
////            }
////        }
//    }
//
//    // angle, energy in Ha
//    public static Map<Double, Double> extractData(Collection<File> files) throws Exception {
//        Map<Double, Double> data = new HashMap<Double, Double>();
//        for (File f : files) {
//            String[] items = f.getName().replace(".g09", "").split("_");
//            int num = Integer.parseInt(items[4]);
//            double angleDeg = 360.0 / 32.0 * num;
//            double angleRad = angleDeg * Math.PI / 180.0;
//            GeometryParser parser = new GeometryParser();
//            parser.set(f);
//            parser.parse();
//            CompChem cc = (CompChem) parser.get();
//            CompChemWrapper ccw = new CompChemWrapper(cc);
//            CMLMolecule m = ccw.getFinalMolecule();
//            CompChemPropertyParser pp = new CompChemPropertyParser();
//            pp.setCompChem(cc);
//            String energy = pp.getProperty("cc:scfenergy", CompChem.COMPCHEM_NS).getValue();
//            data.put(angleRad, Double.parseDouble(energy));
//            System.out.println(angleRad + ", " + energy);
//        }
//        return data;
//    }
}
