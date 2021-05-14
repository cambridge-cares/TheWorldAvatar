/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.tools.structure.util;

import uk.ac.cam.ceb.como.chem.structure.Atom;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.chem.structure.BondType;
import uk.ac.cam.ceb.como.chem.structure.Compound;

/**
 *
 * @author pb556
 */
public class MockCompounds {

    public static Compound getC2H4() {

        Compound c = new Compound();

        Atom c1 = new Atom("C");
        Atom c2 = new Atom("C");
        Atom h1 = new Atom("H");
        Atom h2 = new Atom("H");
        Atom h3 = new Atom("H");
        Atom h4 = new Atom("H");

        c1.setId("a1");
        c2.setId("a2");
        h1.setId("a3");
        h2.setId("a4");
        h3.setId("a5");
        h4.setId("a6");

        // checks position and therefore not only id if you add a new atom
        c1.setCoordinateInA(0.5971, 0.9317, 0.0938);
        c2.setCoordinateInA(1.9285, 0.9160, 0.0536);
        h1.setCoordinateInA(0.0168, 0.0144, 0.1474);
        h2.setCoordinateInA(0.0363, 1.8623, 0.0746);
        h3.setCoordinateInA(2.4893, -0.0146, 0.0727);
        h4.setCoordinateInA(2.5088, 1.8333, -0.0001);

        Bond b1 = Bond.createBond(c1, c2, BondType.DOUBLE);
        Bond b2 = Bond.createBond(c1, h1, BondType.SINGLE);
        Bond b3 = Bond.createBond(c1, h2, BondType.SINGLE);
        Bond b4 = Bond.createBond(c2, h3, BondType.SINGLE);
        Bond b5 = Bond.createBond(c2, h4, BondType.SINGLE);

        c.addAtom(c1);
        c.addAtom(c2);
        c.addAtom(h1);
        c.addAtom(h2);
        c.addAtom(h3);
        c.addAtom(h4);

        c.addBond(b1);
        c.addBond(b2);
        c.addBond(b3);
        c.addBond(b4);
        c.addBond(b5);
        
        c.recreateMoleculeList();

        return c;
    }

    public static Compound getC2H4O() {

        Compound c = new Compound();

        Atom c1 = new Atom("C");
        Atom c2 = new Atom("C");
        Atom h1 = new Atom("H");
        Atom h2 = new Atom("H");
        Atom h3 = new Atom("H");
        Atom h4 = new Atom("H");
        Atom o = new Atom("O");

        c1.setId("a1");
        c2.setId("a2");
        h1.setId("a3");
        h2.setId("a4");
        h3.setId("a5");
        h4.setId("a6");
        o.setId("a7");

        // checks position and therefore not only id if you add a new atom
        c1.setCoordinate(0, 0, 1);
        c2.setCoordinate(0, 0, 2);
        h1.setCoordinate(0, 0, 3);
        h2.setCoordinate(0, 0, 4);
        h3.setCoordinate(0, 0, 5);
        h4.setCoordinate(0, 0, 6);
        o.setCoordinate(0, 0, 7);

        Bond b1 = Bond.createBond(c1, c2, BondType.DOUBLE);
        Bond b2 = Bond.createBond(c1, h1, BondType.SINGLE);
        Bond b3 = Bond.createBond(c1, h2, BondType.SINGLE);
        Bond b4 = Bond.createBond(c2, h3, BondType.SINGLE);
        Bond b5 = Bond.createBond(c2, o, BondType.SINGLE);
        Bond b6 = Bond.createBond(o, h4, BondType.SINGLE);

        c.addAtom(c1);
        c.addAtom(c2);
        c.addAtom(h1);
        c.addAtom(h2);
        c.addAtom(h3);
        c.addAtom(h4);
        c.addAtom(o);

        c.addBond(b1);
        c.addBond(b2);
        c.addBond(b3);
        c.addBond(b4);
        c.addBond(b5);
        c.addBond(b6);

        c.recreateMoleculeList();
        
        return c;
    }

    public static Compound getC2H6() {

        Compound c = new Compound();

        Atom c1 = new Atom("C");
        Atom c2 = new Atom("C");
        Atom h1 = new Atom("H");
        Atom h2 = new Atom("H");
        Atom h3 = new Atom("H");
        Atom h4 = new Atom("H");
        Atom h5 = new Atom("H");
        Atom h6 = new Atom("H");

        c1.setId("a1");
        c2.setId("a2");
        h1.setId("a3");
        h2.setId("a4");
        h3.setId("a5");
        h4.setId("a6");
        h5.setId("a7");
        h6.setId("a8");

        // checks position and therefore not only id if you add a new atom
        c1.setCoordinate(0, 0, 1);
        c2.setCoordinate(0, 0, 2);
        h1.setCoordinate(0, 0, 3);
        h2.setCoordinate(0, 0, 4);
        h3.setCoordinate(0, 0, 5);
        h4.setCoordinate(0, 0, 6);
        h5.setCoordinate(0, 0, 7);
        h6.setCoordinate(0, 0, 8);

        Bond b1 = Bond.createBond(c1, c2, BondType.SINGLE);
        Bond b2 = Bond.createBond(c1, h1, BondType.SINGLE);
        Bond b3 = Bond.createBond(c1, h2, BondType.SINGLE);
        Bond b4 = Bond.createBond(c1, h3, BondType.SINGLE);
        Bond b5 = Bond.createBond(c2, h4, BondType.SINGLE);
        Bond b6 = Bond.createBond(c2, h5, BondType.SINGLE);
        Bond b7 = Bond.createBond(c2, h6, BondType.SINGLE);

        c.addAtom(c1);
        c.addAtom(c2);
        c.addAtom(h1);
        c.addAtom(h2);
        c.addAtom(h3);
        c.addAtom(h4);
        c.addAtom(h5);
        c.addAtom(h6);

        c.addBond(b1);
        c.addBond(b2);
        c.addBond(b3);
        c.addBond(b4);
        c.addBond(b5);
        c.addBond(b6);
        c.addBond(b7);

        c.recreateMoleculeList();
        
        return c;
    }

    public static Compound getC2H6O() {

        Compound c = new Compound();

        Atom c1 = new Atom("C");
        Atom c2 = new Atom("C");
        Atom h1 = new Atom("H");
        Atom h2 = new Atom("H");
        Atom h3 = new Atom("H");
        Atom h4 = new Atom("H");
        Atom h5 = new Atom("H");
        Atom h6 = new Atom("H");
        Atom o = new Atom("O");

        c1.setId("a1");
        c2.setId("a2");
        h1.setId("a3");
        h2.setId("a4");
        h3.setId("a5");
        h4.setId("a6");
        h5.setId("a7");
        h6.setId("a8");
        o.setId("a9");

        // checks position and therefore not only id if you add a new atom
        c1.setCoordinateInA(1.0196, 0.8843, 0.9734);
        c2.setCoordinateInA(1.8755, 1.9905, 1.5708);
        h1.setCoordinateInA(-0.0076, 1.2340, 0.8250);
        h2.setCoordinateInA(0.9989, 0.0153, 1.6382);
        h3.setCoordinateInA(1.4229, 0.5686, 0.0063);
        h4.setCoordinateInA(1.8822, 2.8647, 0.9020);
        h5.setCoordinateInA(1.4572, 2.3102, 2.5375);
        h6.setCoordinateInA(3.7473, 2.1730, 2.1189);
        o.setCoordinateInA(3.1979, 1.4812, 1.7417);

        Bond b1 = Bond.createBond(c1, c2, BondType.SINGLE);
        Bond b2 = Bond.createBond(c1, h1, BondType.SINGLE);
        Bond b3 = Bond.createBond(c1, h2, BondType.SINGLE);
        Bond b4 = Bond.createBond(c1, h3, BondType.SINGLE);
        Bond b5 = Bond.createBond(c2, h4, BondType.SINGLE);
        Bond b6 = Bond.createBond(c2, h5, BondType.SINGLE);
        Bond b7 = Bond.createBond(c2, o, BondType.SINGLE);
        Bond b8 = Bond.createBond(o, h6, BondType.SINGLE);

        c.addAtom(c1);
        c.addAtom(c2);
        c.addAtom(h1);
        c.addAtom(h2);
        c.addAtom(h3);
        c.addAtom(h4);
        c.addAtom(h5);
        c.addAtom(h6);
        c.addAtom(o);

        c.addBond(b1);
        c.addBond(b2);
        c.addBond(b3);
        c.addBond(b4);
        c.addBond(b5);
        c.addBond(b6);
        c.addBond(b7);
        c.addBond(b8);

        c.recreateMoleculeList();
        
        return c;
    }

    public static Compound getC3H6() {

        Compound c = new Compound();

        Atom c1 = new Atom("C");
        Atom c2 = new Atom("C");
        Atom c3 = new Atom("C");
        Atom h1 = new Atom("H");
        Atom h2 = new Atom("H");
        Atom h3 = new Atom("H");
        Atom h4 = new Atom("H");
        Atom h5 = new Atom("H");
        Atom h6 = new Atom("H");

        c1.setId("a1");
        c2.setId("a2");
        c3.setId("a9");
        h1.setId("a3");
        h2.setId("a4");
        h3.setId("a5");
        h4.setId("a6");
        h5.setId("a7");
        h6.setId("a8");

        // checks position and therefore not only id if you add a new atom
        c1.setCoordinate(0, 0, 1);
        c2.setCoordinate(0, 0, 2);
        c3.setCoordinate(0, 0, 9);
        h1.setCoordinate(0, 0, 3);
        h2.setCoordinate(0, 0, 4);
        h3.setCoordinate(0, 0, 5);
        h4.setCoordinate(0, 0, 6);
        h5.setCoordinate(0, 0, 7);
        h6.setCoordinate(0, 0, 8);

        Bond b1 = Bond.createBond(c1, c2, BondType.SINGLE);
        Bond b8 = Bond.createBond(c2, c3, BondType.DOUBLE);
        Bond b2 = Bond.createBond(c1, h1, BondType.SINGLE);
        Bond b3 = Bond.createBond(c1, h2, BondType.SINGLE);
        Bond b4 = Bond.createBond(c1, h3, BondType.SINGLE);
        Bond b5 = Bond.createBond(c2, h4, BondType.SINGLE);
        Bond b6 = Bond.createBond(c3, h5, BondType.SINGLE);
        Bond b7 = Bond.createBond(c3, h6, BondType.SINGLE);

        c.addAtom(c1);
        c.addAtom(c2);
        c.addAtom(c3);
        c.addAtom(h1);
        c.addAtom(h2);
        c.addAtom(h3);
        c.addAtom(h4);
        c.addAtom(h5);
        c.addAtom(h6);

        c.addBond(b1);
        c.addBond(b2);
        c.addBond(b3);
        c.addBond(b4);
        c.addBond(b5);
        c.addBond(b6);
        c.addBond(b7);
        c.addBond(b8);

        c.recreateMoleculeList();
        
        return c;
    }

    public static Compound getC3H8() {
        Compound c = new Compound();

        Atom c1 = new Atom("C");
        Atom c2 = new Atom("C");
        Atom c3 = new Atom("C");
        Atom h1 = new Atom("H");
        Atom h2 = new Atom("H");
        Atom h3 = new Atom("H");
        Atom h4 = new Atom("H");
        Atom h5 = new Atom("H");
        Atom h6 = new Atom("H");
        Atom h7 = new Atom("H");
        Atom h8 = new Atom("H");

        c1.setId("a1");
        c2.setId("a2");
        c3.setId("a3");
        h1.setId("a4");
        h2.setId("a5");
        h3.setId("a6");
        h4.setId("a7");
        h5.setId("a8");
        h6.setId("a9");
        h7.setId("a10");
        h8.setId("a11");

        // checks position and therefore not only id if you add a new atom
        c1.setCoordinate(0, 0, 1);
        c2.setCoordinate(0, 0, 2);
        c2.setCoordinate(0, 0, 9);
        h1.setCoordinate(0, 0, 3);
        h2.setCoordinate(0, 0, 4);
        h3.setCoordinate(0, 0, 5);
        h4.setCoordinate(0, 0, 6);
        h5.setCoordinate(0, 0, 7);
        h6.setCoordinate(0, 0, 8);
        h7.setCoordinate(0, 0, 9);
        h8.setCoordinate(0, 0, 10);

        Bond b1 = Bond.createBond(c1, c2, BondType.SINGLE);
        Bond b2 = Bond.createBond(c2, c3, BondType.SINGLE);

        Bond b3 = Bond.createBond(c1, h1, BondType.SINGLE);
        Bond b4 = Bond.createBond(c1, h2, BondType.SINGLE);
        Bond b5 = Bond.createBond(c1, h3, BondType.SINGLE);

        Bond b6 = Bond.createBond(c2, h6, BondType.SINGLE);
        Bond b7 = Bond.createBond(c2, h7, BondType.SINGLE);
        Bond b8 = Bond.createBond(c2, h8, BondType.SINGLE);

        Bond b9 = Bond.createBond(c3, h4, BondType.SINGLE);
        Bond b10 = Bond.createBond(c3, h5, BondType.SINGLE);

        c.addAtom(c1);
        c.addAtom(c2);
        c.addAtom(c3);
        c.addAtom(h1);
        c.addAtom(h2);
        c.addAtom(h3);
        c.addAtom(h4);
        c.addAtom(h5);
        c.addAtom(h6);
        c.addAtom(h7);
        c.addAtom(h8);

        c.addBond(b1);
        c.addBond(b2);
        c.addBond(b3);
        c.addBond(b4);
        c.addBond(b5);
        c.addBond(b6);
        c.addBond(b7);
        c.addBond(b8);
        c.addBond(b9);
        c.addBond(b10);

        c.recreateMoleculeList();
        
        return c;
    }

    public static Compound getCH4() {
        Compound c = new Compound();

        Atom c1 = new Atom("C");
        Atom h1 = new Atom("H");
        Atom h2 = new Atom("H");
        Atom h3 = new Atom("H");
        Atom h4 = new Atom("H");

        c1.setId("a1");
        h1.setId("a2");
        h2.setId("a3");
        h3.setId("a4");
        h4.setId("a5");

        c1.setCoordinate(0, 0, 1);
        h1.setCoordinate(0, 0, 3);
        h2.setCoordinate(0, 0, 4);
        h3.setCoordinate(0, 0, 5);
        h4.setCoordinate(0, 0, 6);

        Bond b2 = Bond.createBond(c1, h1, BondType.SINGLE);
        Bond b3 = Bond.createBond(c1, h2, BondType.SINGLE);
        Bond b4 = Bond.createBond(c1, h3, BondType.SINGLE);
        Bond b5 = Bond.createBond(c1, h4, BondType.SINGLE);

        c.addAtom(c1);
        c.addAtom(h1);
        c.addAtom(h2);
        c.addAtom(h3);
        c.addAtom(h4);

        c.addBond(b2);
        c.addBond(b3);
        c.addBond(b4);
        c.addBond(b5);

        c.recreateMoleculeList();
        
        return c;
    }

    public static Compound getCH4O() {
        Compound c = new Compound();

        Atom c1 = new Atom("C");
        Atom h1 = new Atom("H");
        Atom h2 = new Atom("H");
        Atom h3 = new Atom("H");
        Atom h4 = new Atom("H");
        Atom o = new Atom("O");

        c1.setId("a1");
        h1.setId("a2");
        h2.setId("a3");
        h3.setId("a4");
        h4.setId("a5");
        o.setId("a6");

        c1.setCoordinate(0, 0, 1);
        o.setCoordinate(0, 0, 2);
        h1.setCoordinate(0, 0, 3);
        h2.setCoordinate(0, 0, 4);
        h3.setCoordinate(0, 0, 5);
        h4.setCoordinate(0, 0, 6);

        Bond b2 = Bond.createBond(c1, h1, BondType.SINGLE);
        Bond b3 = Bond.createBond(c1, h2, BondType.SINGLE);
        Bond b4 = Bond.createBond(c1, h3, BondType.SINGLE);
        Bond b5 = Bond.createBond(c1, o, BondType.SINGLE);
        Bond b6 = Bond.createBond(o, h4, BondType.SINGLE);

        c.addAtom(c1);
        c.addAtom(h1);
        c.addAtom(h2);
        c.addAtom(h3);
        c.addAtom(h4);
        c.addAtom(o);

        c.addBond(b2);
        c.addBond(b3);
        c.addBond(b4);
        c.addBond(b5);
        c.addBond(b6);

        c.recreateMoleculeList();
        
        return c;
    }
}
