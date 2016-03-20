package uk.ac.cam.ceb.como.math.constant;

/**
 *
 * @author pb556
 */
public abstract class PhysicalConstants {
    /**
     * Pressure at 1 atm = 101,325 N/m<sup>2</sup>
     */
    public static final double P_1atm   = 101325;
    /**
     * Avogadro's number = 6.0221417930 &times; 10<sup>23</sup>
     */
    public static final double NA       = 6.022141793E23;
    /**
     * Planck's constant h = 6.62606896(33) &times; 10<sup>-34</sup> J s
     */
    public static final double h        = 6.6260689633E-34;
    /**
     * Reduced Planck's constant (hbar) = h / 2 / pi J s
     */
    public static final double hbar     = h / 2.0 / Math.PI;
    /**
     * Atomic mass unit (amu) = 1.66053878283 &times; 10<sup>-27</sup> kg
     */
    public static final double amu      = 1.66053878283E-27;
    /**
     * Boltzmann's constant k_B = 1.3806504(24) &times; 10<sup>-23</sup> J K<sup>-1</sup>
     */
    public static final double k_B      = 1.380650424E-23;
    /**
     * Speed of ligth c = 299792458 m/s
     */
    public static final double c        = 299792458;
    /**
     * Bohr radius r_Bohr = 0.0529177 &times; 10<sup>-9</sup> m
     */
    public static final double r_Bohr   = 0.0529177E-9;
    /**
     * Angstrom = 1 &times; 10<sup>-10</sup> m
     */
    public static final double A        = 1E-10;
    /**
     * Room temperature at 25 C = 298.15 K
     */
    public static final double T_25C    = 298.15;
    /**
     * Gas constant R
     */
    public static final double R        = NA*k_B;
    /**
     * 1 cal is 4.184 J
     */
    public static final double Cal      = 4.184;
    /**
     * 1 kcal is 4.184 kJ
     */
    public static final double kCal      = 4184;
    /**
     * 1 Hartree engergy is 627509.391 &times; 4.184 J/mol
     */
    public static final double Hartree  = 627509.391*Cal;
    /**
     * 1 atomic MOI
     */
    public static final double AtomicMOI  = amu * r_Bohr * r_Bohr;

}
