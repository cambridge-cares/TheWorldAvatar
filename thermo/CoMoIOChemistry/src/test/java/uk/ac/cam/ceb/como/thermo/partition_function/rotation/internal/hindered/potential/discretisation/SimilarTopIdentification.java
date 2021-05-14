/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation;

/**
 * Identification of a defined top in other molecules.
 *
 * @author pb556
 */
public class SimilarTopIdentification {

//    @Test
//    public void identifyTorsBondsTest() throws Exception {
//
//        String src = "W:\\Data\\TTIP\\hr\\cml\\freq-hr-fine-h-species-0029-radical-1.cml";
//        // extraction from the cml/g09 file
//        CMLHRFileParser parser = new CMLHRFileParser(new File(src));
//        parser.parse();
//        CompChem cc = parser.getCompChem();
//        IRCompChemWrapper ccw = new IRCompChemWrapper(cc);
//        RotationalModes rotModes = ccw.getRotationalModes(true);
//        SMILESBuilder builder = new SMILESBuilder();
//        for (RotationalMode m : rotModes) {
////            try {
////                System.out.println(builder.build(m.getTop().asCMLMolecule()));
////            } catch (Exception e) {
////                System.out.println();
////            }
//            TorsionalBondIdentifier identifier = new TorsionalBondIdentifier(new SMILESBuilder(), m.getTop());
//            Collection<CMLBond> bond = identifier.identify(ccw.getFinalMolecule());
//            System.out.println(bond.size());
//        }
//
//        // read file and check if it contains any possible torsional bonds
////        TorsionalBondIdentifier identifier = new TorsionalBondIdentifier(new SMILESBuilder(), "[CH2][CH2]");
////        
////        String src = "W:\\Data\\TTIP\\hr\\cml\\freq-hr-fine-h-species-0029-radical-1.cml";
////        // extraction from the cml/g09 file
////        CMLHRFileParser parser = new CMLHRFileParser(new File(src));
////        parser.parse();
////        CompChem cc = parser.getCompChem();
////        IRCompChemWrapper ccw = new IRCompChemWrapper(cc);
////        
////        Collection<CMLBond> bond = identifier.identify(ccw.getFinalMolecule());
////        
////        System.out.println(bond.size());
//    }
}
