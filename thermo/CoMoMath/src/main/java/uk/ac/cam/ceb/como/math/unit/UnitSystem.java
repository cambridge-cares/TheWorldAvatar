/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.unit;

/**
 * Enumeration of the unit avialable for printing the object through
 * <code>toString()</code> function.
 *
 *
 */
public enum UnitSystem {
    /**
     * GSD is the unit system used in Gaussian 03 output files.
     * <ul>
     * <li>SCFEnergy in Hartree.</li>
     * <li>Moment of inertia, I, is in amu * r<sup>2</sup><sub>bohr</sub>.</li>
     * <li>All distances and spatial coordinates are in Angstrom (A).</li>
     * <li>Thermochemistry is in cal/mol or kcal/mol.</li>
     * </ul>
     *
     */
    GSD, /**
     * SI is the International System of Units.
     * <ul>
     * <li>All energies are in Jolue (J).</li>
     * <li>All distances and spatial coordinates are in meter (m).</li>
     * <li>Moment of inertia, I, is in kg * m<sup>2</sup>.</li>
     * <li>Thermochemistry is in J/mol or kJ/mol.</li>
     * </ul>
     *
     */ SI

}
