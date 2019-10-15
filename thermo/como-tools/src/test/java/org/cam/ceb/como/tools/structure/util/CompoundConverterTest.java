/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.structure.util;

import gigadot.chom.model.brownie.Atom;
import java.util.ArrayList;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class CompoundConverterTest {

    @Test
    public void convertToEmpiricalFormulaTest() {
        
        assert (CompoundConverter.convertToEmpiricalFormula(MockCompounds.getC2H4()).equals("C2H4"));
        assert (CompoundConverter.convertToEmpiricalFormula(MockCompounds.getC2H4O()).equals("C2H4O"));
        assert (CompoundConverter.convertToEmpiricalFormula(MockCompounds.getC2H6()).equals("C2H6"));
        assert (CompoundConverter.convertToEmpiricalFormula(MockCompounds.getC2H6O()).equals("C2H6O"));
        assert (CompoundConverter.convertToEmpiricalFormula(MockCompounds.getC3H6()).equals("C3H6"));
        assert (CompoundConverter.convertToEmpiricalFormula(MockCompounds.getC3H8()).equals("C3H8"));
        assert (CompoundConverter.convertToEmpiricalFormula(MockCompounds.getCH4()).equals("CH4"));
        assert (CompoundConverter.convertToEmpiricalFormula(MockCompounds.getCH4O()).equals("CH4O"));

        ArrayList<Atom> atoms = new ArrayList<Atom>();
        atoms.add(new Atom("O"));
        atoms.add(new Atom("C"));
        atoms.add(new Atom("C"));
        atoms.add(new Atom("H"));
        atoms.add(new Atom("H"));
        atoms.add(new Atom("H"));
        atoms.add(new Atom("H"));
        assert (CompoundConverter.convertToEmpiricalFormula(atoms).equals("CCHHHHO"));

        atoms = new ArrayList<Atom>();
        atoms.add(new Atom("C"));
        atoms.add(new Atom("H"));
        atoms.add(new Atom("H"));
        atoms.add(new Atom("H"));
        atoms.add(new Atom("H"));
        assert (CompoundConverter.convertToEmpiricalFormula(atoms).equals("CHHHH"));

        atoms = new ArrayList<Atom>();
        atoms.add(new Atom("C"));
        atoms.add(new Atom("C"));
        atoms.add(new Atom("H"));
        atoms.add(new Atom("H"));
        atoms.add(new Atom("H"));
        atoms.add(new Atom("H"));
        assert (CompoundConverter.convertToEmpiricalFormula(atoms).equals("CCHHHH"));

        assert (CompoundConverter.convertToEmpiricalFormula(new Atom[]{
            new Atom("C"), new Atom("C"),
            new Atom("H"), new Atom("H"),
            new Atom("H"), new Atom("H"),
            new Atom("O")}).equals("CCHHHHO"));

        assert (CompoundConverter.convertToEmpiricalFormula(new Atom[]{
            new Atom("C"), new Atom("H"),
            new Atom("H"), new Atom("H"),
            new Atom("H")}).equals("CHHHH"));

        assert (CompoundConverter.convertToEmpiricalFormula(new Atom[]{
            new Atom("C"), new Atom("C"),
            new Atom("H"), new Atom("H"),
            new Atom("H"), new Atom("H")}).equals("CCHHHH"));
    }

    @Test
    public void compressEmpiricalFormulaTest() {
        assert (CompoundConverter.compressEmpiricalFormula("CCHHHH").equals("C2H4"));
        assert (CompoundConverter.compressEmpiricalFormula("CCCHHHHHHHH").equals("C3H8"));
        assert (CompoundConverter.compressEmpiricalFormula("CCHHHOHHH").equals("C2H3OH3"));
    }

    @Test
    public void convertStructureToCMLTest() {

        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H4(), CompoundConverter.convert(MockCompounds.getC2H4())));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H4O(), CompoundConverter.convert(MockCompounds.getC2H4O())));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H6(), CompoundConverter.convert(MockCompounds.getC2H6())));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H6O(), CompoundConverter.convert(MockCompounds.getC2H6O())));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC3H6(), CompoundConverter.convert(MockCompounds.getC3H6())));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC3H8(), CompoundConverter.convert(MockCompounds.getC3H8())));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getCH4(), CompoundConverter.convert(MockCompounds.getCH4())));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getCH4O(), CompoundConverter.convert(MockCompounds.getCH4O())));

        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H4(), CompoundConverter.convert(MockCompounds.getCH4O())));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H4O(), CompoundConverter.convert(MockCompounds.getC2H4())));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H6(), CompoundConverter.convert(MockCompounds.getC3H6())));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H6O(), CompoundConverter.convert(MockCompounds.getC2H4O())));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC3H6(), CompoundConverter.convert(MockCompounds.getC2H4())));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC3H8(), CompoundConverter.convert(MockCompounds.getC2H6O())));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getCH4(), CompoundConverter.convert(MockCompounds.getC2H4())));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getCH4O(), CompoundConverter.convert(MockCompounds.getCH4())));
    }

    @Test
    public void convertMoleculeToCMLTest() {
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H4(), CompoundConverter.convert(MockCompounds.getC2H4().getMolecule(0))));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H4O(), CompoundConverter.convert(MockCompounds.getC2H4O().getMolecule(0))));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H6(), CompoundConverter.convert(MockCompounds.getC2H6().getMolecule(0))));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H6O(), CompoundConverter.convert(MockCompounds.getC2H6O().getMolecule(0))));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC3H6(), CompoundConverter.convert(MockCompounds.getC3H6().getMolecule(0))));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC3H8(), CompoundConverter.convert(MockCompounds.getC3H8().getMolecule(0))));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getCH4(), CompoundConverter.convert(MockCompounds.getCH4().getMolecule(0))));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getCH4O(), CompoundConverter.convert(MockCompounds.getCH4O().getMolecule(0))));

        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H4(), CompoundConverter.convert(MockCompounds.getCH4O().getMolecule(0))));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H4O(), CompoundConverter.convert(MockCompounds.getC2H4().getMolecule(0))));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H6(), CompoundConverter.convert(MockCompounds.getC3H6().getMolecule(0))));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H6O(), CompoundConverter.convert(MockCompounds.getC2H4O().getMolecule(0))));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC3H6(), CompoundConverter.convert(MockCompounds.getC2H4().getMolecule(0))));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC3H8(), CompoundConverter.convert(MockCompounds.getC2H6O().getMolecule(0))));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getCH4(), CompoundConverter.convert(MockCompounds.getC2H4().getMolecule(0))));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getCH4O(), CompoundConverter.convert(MockCompounds.getCH4().getMolecule(0))));
    }

    @Test
    public void convertCMLToCompoundTest() {
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H4(), CompoundConverter.convert(MockCMLMolecules.getC2H4())));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H6O(), CompoundConverter.convert(MockCMLMolecules.getC2H6O())));

        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H4O(), CompoundConverter.convert(MockCMLMolecules.getC2H4())));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC3H6(), CompoundConverter.convert(MockCMLMolecules.getC2H4())));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC3H8(), CompoundConverter.convert(MockCMLMolecules.getC2H6O())));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getCH4(), CompoundConverter.convert(MockCMLMolecules.getC2H4())));
    }

    @Test
    public void convertCMLToBondTest() {
        // tested with previous methods
        assert(true);
    }

    @Test
    public void convertCMLToAtomTest() {
        // tested with previous methods
        assert(true);
    }
}
