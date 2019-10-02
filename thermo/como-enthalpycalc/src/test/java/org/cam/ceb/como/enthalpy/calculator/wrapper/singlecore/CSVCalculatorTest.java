/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore;

import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.LpSolverException;
import org.cam.ceb.como.enthalpy.calculator.solver.glpk.TerminalGLPKSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.lpsolve.TerminalLPSolveSolver;
import java.io.File;
import java.util.HashMap;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class CSVCalculatorTest {

//    @Test
//    public void calculateISDGLPKTest() throws LpSolverException {
//        LPSolver solver = new TerminalGLPKSolver(true, true);
//        solver.setDirectory(new File("test_data/"));
//        CSVCalculator calc = new CSVISDCalculator(
//                solver, new ISDMPSFormat());
//        calc.setCSV(new File("test_data/csv/ref.csv"), new File("test_data/csv/soi.csv"));
//        HashMap<Species, ReactionList> reactionList = (HashMap<Species, ReactionList>) calc.calculate();
//        for (Species s : reactionList.keySet()) {
//            Reaction r = reactionList.get(s);
//                if (s.getRef().toString().equals("CH4_SOI")) {
//                    assert (reactions.get(s).calculateHf() == -74.87);
//                } else if (s.getRef().toString().equals("C2H6O_SOI")) {
//                    assert (reactions.get(s).calculateHf() == -234.0);
//                } else {
//                    assert (false);
//                }
//            }
//        }
//
//        Reaction reaction = calc.calculate(MockISDSpecies.getC2H4());
//        assert (reaction.calculateHf() == 52.47);
//        reaction = calc.calculate(MockISDSpecies.getC2H4O());
//        assert (reaction.calculateHf() == -128.0);
//        reaction = calc.calculate(MockISDSpecies.getC2H6());
//        assert (reaction.calculateHf() == -83.8);
//        reaction = calc.calculate(MockISDSpecies.getC2H6O());
//        assert (reaction.calculateHf() == -234.0);
//        reaction = calc.calculate(MockISDSpecies.getC3H6());
//        assert (reaction.calculateHf() == 20.41);
//        reaction = calc.calculate(MockISDSpecies.getC3H8());
//        System.out.println(reaction.calculateHf());
//        assert (reaction.calculateHf() == -104.7);
//        reaction = calc.calculate(MockISDSpecies.getCH4());
//        assert (reaction.calculateHf() == -74.87);
//        reaction = calc.calculate(MockISDSpecies.getCH4O());
//        assert (reaction.calculateHf() == -205.0);
//
//        calc = new CSVISDCalculator(
//                solver, new ISDMPSFormat(),
//                new File("test_data/csv/ref_missing.csv"));
//        reactions = calc.calculate(new File("test_data/csv/soi.csv"));
//        for (Species s : reactions.keySet()) {
//            if (s.getRef().toString().equals("CH4_SOI")) {
//                assert (reactions.get(s).calculateHf() <= -74.87 + 20 && reactions.get(s).calculateHf() >= -74.87 - 20);
//            } else if (s.getRef().toString().equals("C2H6O_SOI")) {
//                assert (reactions.get(s).calculateHf() <= -234.0 + 20 && reactions.get(s).calculateHf() >= -234.0 - 20);
//            } else {
//                assert (false);
//            }
//        }
//
//        reaction = calc.calculate(MockISDSpecies.getC2H4());
//        assert (reaction.calculateHf() == 52.47);
//        reaction = calc.calculate(MockISDSpecies.getC2H4O());
//        assert (reaction.calculateHf() == -128.0);
//        reaction = calc.calculate(MockISDSpecies.getC2H6());
//        assert (reaction.calculateHf() == -83.8);
//        reaction = calc.calculate(MockISDSpecies.getC2H6O());
//        assert (reaction.calculateHf() <= -234.0 + 0.1 * 234.0 && reaction.calculateHf() >= -234.0 - 0.1 * 234.0);
//        //assert (reaction.calculateHf() == -234.0);
//        reaction = calc.calculate(MockISDSpecies.getC3H6());
//        assert (reaction.calculateHf() == 20.41);
//        reaction = calc.calculate(MockISDSpecies.getC3H8());
//        assert (reaction.calculateHf() == -104.7);
//        reaction = calc.calculate(MockISDSpecies.getCH4());
//        assert (reaction.calculateHf() <= -74.87 + 74.87 * 0.1 && reaction.calculateHf() >= -74.87 - 74.87 * 0.1);
//        //assert (reaction.calculateHf() == -74.87);
//        reaction = calc.calculate(MockISDSpecies.getCH4O());
//        assert (reaction.calculateHf() == -205.0);
//
//    }
//
//    @Test
//    public void calculateISDLPSolveTest() throws LpSolverException {
//        LPSolver solver = new TerminalLPSolveSolver(true, true);
//        solver.setDirectory(new File("test_data/"));
//        CSVCalculator calc = new CSVISDCalculator(
//                solver, new ISDLpSolveFormat(),
//                new File("test_data/csv/ref.csv"));
//        HashMap<Species, Reaction> reactions = calc.calculate(new File("test_data/csv/soi.csv"));
//        for (Species s : reactions.keySet()) {
//            if (s.getRef().toString().equals("CH4_SOI")) {
//                assert (reactions.get(s).calculateHf() == -74.87);
//            } else if (s.getRef().toString().equals("C2H6O_SOI")) {
//                assert (reactions.get(s).calculateHf() == -234.0);
//            } else {
//                assert (false);
//            }
//        }
//
//        Reaction reaction = calc.calculate(MockISDSpecies.getC2H4());
//        assert (reaction.calculateHf() == 52.47);
//        reaction = calc.calculate(MockISDSpecies.getC2H4O());
//        assert (reaction.calculateHf() == -128.0);
//        reaction = calc.calculate(MockISDSpecies.getC2H6());
//        assert (reaction.calculateHf() == -83.8);
//        reaction = calc.calculate(MockISDSpecies.getC2H6O());
//        assert (reaction.calculateHf() == -234.0);
//        reaction = calc.calculate(MockISDSpecies.getC3H6());
//        assert (reaction.calculateHf() == 20.41);
//        reaction = calc.calculate(MockISDSpecies.getC3H8());
//        assert (reaction.calculateHf() == -104.7);
//        reaction = calc.calculate(MockISDSpecies.getCH4());
//        assert (reaction.calculateHf() == -74.87);
//        reaction = calc.calculate(MockISDSpecies.getCH4O());
//        assert (reaction.calculateHf() == -205.0);
//
//        calc = new CSVISDCalculator(
//                solver, new ISDLpSolveFormat(),
//                new File("test_data/csv/ref_missing.csv"));
//        reactions = calc.calculate(new File("test_data/csv/soi.csv"));
//        for (Species s : reactions.keySet()) {
//            if (s.getRef().toString().equals("CH4_SOI")) {
//                assert (reactions.get(s).calculateHf() <= -74.87 + 20 && reactions.get(s).calculateHf() >= -74.87 - 20);
//            } else if (s.getRef().toString().equals("C2H6O_SOI")) {
//                assert (reactions.get(s).calculateHf() <= -234.0 + 20 && reactions.get(s).calculateHf() >= -234.0 - 20);
//            } else {
//                assert (false);
//            }
//        }
//
//        reaction = calc.calculate(MockISDSpecies.getC2H4());
//        assert (reaction.calculateHf() == 52.47);
//        reaction = calc.calculate(MockISDSpecies.getC2H4O());
//        assert (reaction.calculateHf() == -128.0);
//        reaction = calc.calculate(MockISDSpecies.getC2H6());
//        assert (reaction.calculateHf() == -83.8);
//        reaction = calc.calculate(MockISDSpecies.getC2H6O());
//        assert (reaction.calculateHf() <= -234.0 + 234 * 0.1 && reaction.calculateHf() >= -234.0 - 234 * 0.1);
//        //assert (reaction.calculateHf() == -234.0);
//        reaction = calc.calculate(MockISDSpecies.getC3H6());
//        assert (reaction.calculateHf() == 20.41);
//        reaction = calc.calculate(MockISDSpecies.getC3H8());
//        assert (reaction.calculateHf() == -104.7);
//        reaction = calc.calculate(MockISDSpecies.getCH4());
//        assert (reaction.calculateHf() <= -74.87 + 0.2 * 74.87 && reaction.calculateHf() >= -74.87 - 0.2 * 74.87);
//        //assert (reaction.calculateHf() == -74.87);
//        reaction = calc.calculate(MockISDSpecies.getCH4O());
//        assert (reaction.calculateHf() == -205.0);
//
//    }
//
//    @Test
//    public void calculateISGGLPKTest() throws LpSolverException {
//        LPSolver solver = new TerminalGLPKSolver(true, true);
//        solver.setDirectory(new File("test_data/"));
//        CSVCalculator calc = new CSVISGCalculator(
//                solver, new ISGMPSFormat(),
//                new File("test_data/csv/ref.csv"));
//        HashMap<Species, Reaction> reactions = calc.calculate(new File("test_data/csv/soi.csv"));
//        for (Species s : reactions.keySet()) {
//            if (s.getRef().toString().equals("CH4_SOI")) {
//                assert (reactions.get(s).calculateHf() == -74.87);
//            } else if (s.getRef().toString().equals("C2H6O_SOI")) {
//                assert (reactions.get(s).calculateHf() == -234.0);
//            } else {
//                assert (false);
//            }
//        }
//
//        Reaction reaction = calc.calculate(MockISDSpecies.getC2H4());
//        assert (reaction.calculateHf() == 52.47);
//        reaction = calc.calculate(MockISDSpecies.getC2H4O());
//        assert (reaction.calculateHf() == -128.0);
//        reaction = calc.calculate(MockISDSpecies.getC2H6());
//        assert (reaction.calculateHf() == -83.8);
//        reaction = calc.calculate(MockISDSpecies.getC2H6O());
//        assert (reaction.calculateHf() == -234.0);
//        reaction = calc.calculate(MockISDSpecies.getC3H6());
//        assert (reaction.calculateHf() == 20.41);
//        reaction = calc.calculate(MockISDSpecies.getC3H8());
//        System.out.println(reaction.calculateHf());
//        //assert (reaction.calculateHf() == -104.7);
//        assert (reaction.calculateHf() <= -104.7 + 10 && reaction.calculateHf() >= -104.7 - 10);
//        reaction = calc.calculate(MockISDSpecies.getCH4());
//        assert (reaction.calculateHf() == -74.87);
//        reaction = calc.calculate(MockISDSpecies.getCH4O());
//        assert (reaction.calculateHf() == -205.0);
//
//        calc = new CSVISGCalculator(
//                solver, new ISGMPSFormat(),
//                new File("test_data/csv/ref_missing.csv"));
//        reactions = calc.calculate(new File("test_data/csv/soi.csv"));
//        for (Species s : reactions.keySet()) {
//            if (s.getRef().toString().equals("CH4_SOI")) {
//                System.out.println(reactions.get(s).calculateHf());
//                assert (reactions.get(s).calculateHf() <= -74.87 + 20 && reactions.get(s).calculateHf() >= -74.87 - 20);
//            } else if (s.getRef().toString().equals("C2H6O_SOI")) {
//                System.out.println(reactions.get(s).calculateHf());
//                assert (reactions.get(s).calculateHf() <= -234.0 + 20 && reactions.get(s).calculateHf() >= -234.0 - 20);
//            } else {
//                assert (false);
//            }
//        }
//
//        reaction = calc.calculate(MockISDSpecies.getC2H4());
//        assert (reaction.calculateHf() == 52.47);
//        reaction = calc.calculate(MockISDSpecies.getC2H4O());
//        assert (reaction.calculateHf() == -128.0);
//        reaction = calc.calculate(MockISDSpecies.getC2H6());
//        assert (reaction.calculateHf() == -83.8);
//        reaction = calc.calculate(MockISDSpecies.getC2H6O());
//        assert (reaction.calculateHf() <= -234.0 + 10 && reaction.calculateHf() >= -234.0 - 10);
//        //System.out.println(reaction.calculateHf());
//        //assert (reaction.calculateHf() == -234.0);
//        reaction = calc.calculate(MockISDSpecies.getC3H6());
//        assert (reaction.calculateHf() == 20.41);
//        reaction = calc.calculate(MockISDSpecies.getC3H8());
//        assert (reaction.calculateHf() == -104.7);
//        reaction = calc.calculate(MockISDSpecies.getCH4());
//        assert (reaction.calculateHf() <= -74.87 + 10 && reaction.calculateHf() >= -74.87 - 10);
//        //assert (reaction.calculateHf() == -74.87);
//        reaction = calc.calculate(MockISDSpecies.getCH4O());
//        assert (reaction.calculateHf() == -205.0);
//
//    }
//
//    @Test
//    public void calculateISGLPSolveTest() throws LpSolverException {
//        LPSolver solver = new TerminalLPSolveSolver(true, true);
//        solver.setDirectory(new File("test_data/"));
//        CSVCalculator calc = new CSVISGCalculator(
//                solver, new ISGLpSolveFormat(),
//                new File("test_data/csv/ref.csv"));
//        HashMap<Species, Reaction> reactions = calc.calculate(new File("test_data/csv/soi.csv"));
//        for (Species s : reactions.keySet()) {
//            if (s.getRef().toString().equals("CH4_SOI")) {
//                assert (reactions.get(s).calculateHf() <= -74.87 + 20 && reactions.get(s).calculateHf() >= -74.87 - 20);
//            } else if (s.getRef().toString().equals("C2H6O_SOI")) {
//                assert (reactions.get(s).calculateHf() <= -234.0 + 20 && reactions.get(s).calculateHf() >= -234.0 - 20);
//            } else {
//                assert (false);
//            }
//        }
//
//        Reaction reaction = calc.calculate(MockISDSpecies.getC2H4());
//        assert (reaction.calculateHf() == 52.47);
//        reaction = calc.calculate(MockISDSpecies.getC2H4O());
//        assert (reaction.calculateHf() <= -128.0 + 128.0 * 0.1 && reaction.calculateHf() >= -128.0 - 128.0 * 0.1);
////        reaction = calc.calculate(MockISDSpecies.getC2H6());
////        System.out.println(reaction.calculateHf());
////        assert (reaction.calculateHf() <= -83.8 + 83.8 * 0.1 && reaction.calculateHf() >= -83.8 - 83.8 * 0.1);
//        //assert (reaction.calculateHf() == -83.8);
//        reaction = calc.calculate(MockISDSpecies.getC2H6O());
//        //assert (reaction.calculateHf() <= -234.0 + 20 && reaction.calculateHf() >= -234.0 - 20);
//        assert (reaction.calculateHf() <= -234.0 + 234.0 * 0.1 && reaction.calculateHf() >= -234.0 - 234.0 * 0.1);
//        reaction = calc.calculate(MockISDSpecies.getC3H6());
//        //System.out.println(reaction.calculateHf());
//        assert (reaction.calculateHf() <= 20.41 + 20.41 * 0.2 && reaction.calculateHf() >= 20.41 - 20.41 * 0.2);
//        reaction = calc.calculate(MockISDSpecies.getC3H8());
//        assert (reaction.calculateHf() == -104.7);
//        reaction = calc.calculate(MockISDSpecies.getCH4());
//        assert (reaction.calculateHf() == -74.87);
//        reaction = calc.calculate(MockISDSpecies.getCH4O());
//        assert (reaction.calculateHf() == -205.0);
//
//        calc = new CSVISGCalculator(
//                solver, new ISGLpSolveFormat(),
//                new File("test_data/csv/ref_missing.csv"));
//        reactions = calc.calculate(new File("test_data/csv/soi.csv"));
//        for (Species s : reactions.keySet()) {
//            if (s.getRef().toString().equals("CH4_SOI")) {
//                assert (reactions.get(s).calculateHf() <= -74.87 + 20 && reactions.get(s).calculateHf() >= -74.87 - 20);
//            } else if (s.getRef().toString().equals("C2H6O_SOI")) {
//                assert (reactions.get(s).calculateHf() <= -234.0 + 20 && reactions.get(s).calculateHf() >= -234.0 - 20);
//            } else {
//                assert (false);
//            }
//        }
//
//        reaction = calc.calculate(MockISDSpecies.getC2H4());
//        assert (reaction.calculateHf() == 52.47);
//        reaction = calc.calculate(MockISDSpecies.getC2H4O());
//        assert (reaction.calculateHf() == -128.0);
//        reaction = calc.calculate(MockISDSpecies.getC2H6());
//        assert (reaction.calculateHf() == -83.8);
//        reaction = calc.calculate(MockISDSpecies.getC2H6O());
//        assert (reaction.calculateHf() <= -234.0 + 0.1 * 234.0 && reaction.calculateHf() >= -234.0 - 0.1 * 234.0);
//        //assert (reaction.calculateHf() == -234.0);
//        reaction = calc.calculate(MockISDSpecies.getC3H6());
//        assert (reaction.calculateHf() <= 20.41 + 20.41 * 0.2 && reaction.calculateHf() >= 20.41 - 20.41 * 0.2);
//        reaction = calc.calculate(MockISDSpecies.getC3H8());
//        assert (reaction.calculateHf() <= -104.7 + 0.1 * 104.7 && reaction.calculateHf() >= -104.7 - 0.1 * 104.7);
//        //assert (reaction.calculateHf() == -104.7);
//        reaction = calc.calculate(MockISDSpecies.getCH4());
//        //assert (reaction.calculateHf() <= -74.87 + 20 && reaction.calculateHf() >= -74.87 - 20);
//        //assert (reaction.calculateHf() == -74.87);
//        System.out.println(reaction.calculateHf());
//        assert (reaction.calculateHf() <= -74.87 + 74.87 * 0.2 && reaction.calculateHf() >= -74.87 - 74.87 * 0.2);
//        reaction = calc.calculate(MockISDSpecies.getCH4O());
//        assert (reaction.calculateHf() == -205.0);
//
//    }
}
