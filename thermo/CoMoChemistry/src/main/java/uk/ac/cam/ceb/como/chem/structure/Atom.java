package uk.ac.cam.ceb.como.chem.structure;

import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.chem.periodictable.PeriodicTable;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;

/**
 *
 * @author pb556
 */
public class Atom {

    private static final double db_precision = 0.0001 * PhysicalConstants.A;
    /**
     * Order in the molecular document. Must be unique.
     */
    private int order = -1;
    /**
     * Positions X in m.
     */
    private double posX = 0.0;
    /**
     * Positions Y in m.
     */
    private double posY = 0.0;
    /**
     * Positions Z in m.
     */
    private double posZ = 0.0;
    /**
     * Positions X in A.
     */
    private double posXInA = 0.0;
    /**
     * Positions Y in A.
     */
    private double posYInA = 0.0;
    
    /**
     * Positions Z in A.
     */
    
    private double posZInA = 0.0;
    private Element element = null;
    private Integer isotope = null;
    private int count = 1;

    private static final String molDocThisVersion = "1.0";
    private String id = null;
    private String ref = null;
    private Long version;
    private String molDocVersion = molDocThisVersion;
    
    protected Atom() {
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getRef() {
        return ref;
    }

    public void setRef(String ref) {
        this.ref = ref;
    }

    public Long getVersion() {
        return version;
    }

    public void setVersion(Long version) {
        this.version = version;
    }

    protected String getMolDocVersion() {
        return molDocVersion;
    }

    protected void setMolDocVersion(String version) {
        this.molDocVersion = version;
    }

    protected String getElementSymbol() {
        return element.getSymbol();
    }

    protected void setElementSymbol(String elementSymbol) {
        this.element = PeriodicTable.getElementBySymbol(elementSymbol);
    }

    /**
     * Get the value of count
     *
     * @return the value of count
     */
    public int getCount() {
        return count;
    }

    /**
     * Set the value of count
     *
     * @param count new value of count
     */
    public void setCount(int count) {
        this.count = count;
    }

    /**
     * Get the value of isotope.
     *
     * @return the value of isotope. null if not set.
     */
    public Integer getIsotope() {
        return isotope;
    }

    /**
     * Set the value of isotope
     *
     * @param isotope new value of isotope
     */
    public void setIsotope(Integer isotope) {
        this.isotope = isotope;
    }

    /**
     * Use Atom.createAtom or StructureTools.createAtom to create atom if you don't know what you are doing. we will
     * make move the factory method here if it is appropriated.
     *
     * @param elem
     */
    public Atom(Element elem) {
        this.element = elem;
    }

    public Atom(String elementSymbol) {
        this.element = PeriodicTable.getElementBySymbol(elementSymbol);
    }

    public Atom(int Z) {
        this.element = PeriodicTable.getElementByAtomicNumber(Z);
    }

    public Element getElement() {
        return element;
    }

    public boolean is(String symbol) {
        return element.getSymbol().equalsIgnoreCase(symbol);
    }

    public boolean is(Element elem) {
        return element.equals(elem);
    }

    public boolean isAtomicNumber(int Z) {
        return element.getAtomicNumber() == Z;
    }

    public int getOrder() {
        return order;
    }

    public double getXInA() {
        return posXInA;
    }

    public double getYInA() {
        return posYInA;
    }

    public double getZInA() {
        return posZInA;
    }

    public void setCoordinateInA(double x, double y, double z) {
        posX = x * PhysicalConstants.A;
        posY = y * PhysicalConstants.A;
        posZ = z * PhysicalConstants.A;
        posXInA = x;
        posYInA = y;
        posZInA = z;
    }

    public void setXInA(double x) {
        posX = x * PhysicalConstants.A;
        posXInA = x;
    }

    public void setYInA(double y) {
        posY = y * PhysicalConstants.A;
        posYInA = y;
    }

    public void setZInA(double z) {
        posZ = z * PhysicalConstants.A;
        posZInA = z;
    }

    public double getX() {
        return posX;
    }

    public double getY() {
        return posY;
    }

    public double getZ() {
        return posZ;
    }

    public void setCoordinate(double x, double y, double z) {
        posX = x;
        posY = y;
        posZ = z;
        posXInA = x / PhysicalConstants.A;
        posYInA = y / PhysicalConstants.A;
        posZInA = z / PhysicalConstants.A;
    }

    public void setOrder(int order) {
        this.order = order;
    }

    /**
     * Two Atom are equal if they are made of the same element and have the same position with in the limit of double
     * precision.
     *
     * @param obj
     * @return
     */
    //@Override
    public boolean equalsIgnoreOrder(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof Atom)) {
            return false;
        }
        // Object must be Atom at this point
        Atom atom = (Atom) obj;
        return ((this.element.equals(atom.element))
                && (Math.abs(this.posX - atom.posX) < db_precision)
                && (Math.abs(this.posY - atom.posY) < db_precision)
                && (Math.abs(this.posZ - atom.posZ) < db_precision));
    }

    /**
     * Two Atom are equal if they are made of the same element and have the same order.
     *
     * @param obj
     * @return
     */
    public boolean equalsIgnorePosition(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof Atom)) {
            return false;
        }
        // Object must be Atom at this point
        Atom atom = (Atom) obj;
        return ((this.element.equals(atom.element))
                && (this.order == atom.order));
    }

    /**
     * Two Atom are equal if all properties are the same.
     *
     * @param obj
     * @return
     */
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof Atom)) {
            return false;
        }
        // Object must be Atom at this point
        Atom atom = (Atom) obj;
        return ((this.element.equals(atom.element))
                && (this.order == atom.order)
                && (Math.abs(this.posX - atom.posX) < db_precision)
                && (Math.abs(this.posY - atom.posY) < db_precision)
                && (Math.abs(this.posZ - atom.posZ) < db_precision));
    }

    @Override
    public int hashCode() {
        int hash = 5;
        hash = 13 * hash + this.order;
        hash = 13 * hash + (int) (Double.doubleToLongBits(this.posX) ^ (Double.doubleToLongBits(this.posX) >>> 32));
        hash = 13 * hash + (int) (Double.doubleToLongBits(this.posY) ^ (Double.doubleToLongBits(this.posY) >>> 32));
        hash = 13 * hash + (int) (Double.doubleToLongBits(this.posZ) ^ (Double.doubleToLongBits(this.posZ) >>> 32));
        hash = 13 * hash + (this.element != null ? this.element.hashCode() : 0);
        return hash;
    }

    @Override
    public String toString() {
        return "Symbol : " + element.getSymbol() + ", Order : " + order + ", Position : [" + posX + ", " + posY + ", " + posZ + "]";
    }
}