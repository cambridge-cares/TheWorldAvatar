package org.cam.ceb.como.tools.periodictable.old;

/**
 *
 * @author pb556
 */
public class ElementOld {

    protected String name;
    /**
     * Atomic symbol
     */
    protected String symbol;
    /**
     * A = Z + N
     */
    protected int massNumber;
    /**
     * Z number of proton
     */
    protected int atomicNumber;
    /**
     * Relative atomic mass to 1/12 of C12
     */
    protected double atomicWeight;
    protected double meltingPoint;
    protected double boilingPoint;
    protected int numNeutrons;
    protected double density;
    protected int row;
    protected int col;
    protected CLASSIFICATION classification;

    public enum CLASSIFICATION {

        NONMETAL, NOBLEGAS, ALKALIMETAL, ALKANIEARTH, METALLOID, HALOGEN, OTHERMETALS, ALKALINEEARTH, TRANSITIONMETAL, RAREEARTH_LANTHANIDES, RAREEARTH_ACTINIDES
    }

    /**
     * Protected constructor, only class with in the same package can have
     * access to call this constructor. This is intended for PeriodicTable class
     * to be used.
     *
     * @param symbol
     * @param massNumber
     * @param atomicNumber
     * @param atomicWeight
     */
    public ElementOld() {
    }

    public ElementOld(String symbol, String name) {
        this.name = name;
        this.symbol = symbol;
    }

    public ElementOld(String symbol, int massNumber, int atomicNumber, double atomicWeight) {
        this.symbol = symbol;
        this.massNumber = massNumber;
        this.atomicNumber = atomicNumber;
        this.atomicWeight = atomicWeight;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setSymbol(String symbol) {
        this.symbol = symbol;
    }

    public void setAtomicNumber(int atomicNumber) {
        this.atomicNumber = atomicNumber;
    }

    public void setAtomicWeight(double atomicWeight) {
        this.atomicWeight = atomicWeight;
    }
    
    public void setBoilingPoint(double boilingPoint) {
        this.boilingPoint = boilingPoint;
    }
    
    public void setMeltingPoint(double meltingPoint) {
        this.meltingPoint = meltingPoint;
    }

//    public void setMassNumber(int massNumber) {
//        this.massNumber = massNumber;
//    }

    public void setNumberOfNeutrons(int numNeutrons) {
        this.numNeutrons = numNeutrons;
    }

    public void setClassification(CLASSIFICATION classification) {
        this.classification = classification;
    }

    public void setDensity(double density) {
        this.density = density;
    }

    public void setRow(int row) {
        this.row = row;
    }

    public void setColumn(int col) {
        this.col = col;
    }

    public String getName() {
        return name;
    }

    public String getSymbol() {
        return symbol;
    }

    public int getAtomicNumber() {
        return atomicNumber;
    }

    public double getAtomicWeight() {
        return atomicWeight;
    }

    public int getMassNumber() {
        massNumber = numNeutrons + atomicNumber;
        return massNumber;
    }

    public int getNumberOfNeutrons() {
        return numNeutrons;
    }
    
    public double getBoilingPoint() {
        return boilingPoint;
    }
    
    public double getMeltingPoint() {
        return meltingPoint;
    }

    public CLASSIFICATION getClassification() {
        return classification;
    }

    public double getDensity() {
        return density;
    }

    public int getRow() {
        return row;
    }

    public int getColumn() {
        return col;
    }

    /**
     * Two Element(s) are equal if their atomic number are the same but the
     * count number can be different. Currently, not supporting the isotope.
     *
     * @param obj
     * @return
     */
    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ElementOld)) {
            return false;
        }

        //cast to native object is now safe
        ElementOld eObj = (ElementOld) obj;

        //now a proper field-by-field evaluation can be made
        return ((this.symbol == null ? eObj.symbol == null : this.symbol.equals(eObj.symbol)) && this.atomicNumber == eObj.atomicNumber && this.massNumber == eObj.massNumber);
    }

    @Override
    public int hashCode() {
        int hash = 5;
        hash = 61 * hash + (this.symbol != null ? this.symbol.hashCode() : 0);
        hash = 61 * hash + this.massNumber;
        hash = 61 * hash + this.atomicNumber;
        return hash;
    }
}
