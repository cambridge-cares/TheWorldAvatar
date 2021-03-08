package gigadot.chom.chem.property;

import gigadot.chom.model.property.CompositeProperty;
import gigadot.chom.model.enumeration.UnitSystem;
import gigatools.lite.constant.PhysicalConstants;
import java.text.DecimalFormat;
import org.apache.commons.lang.StringUtils;

/**
 *
 * @author Weerapong Phadungsukanan
 */
public class Displacement extends CompositeProperty {

    private double dxInA = 0.0;
    private double dyInA = 0.0;
    private double dzInA = 0.0;
    private double dx = 0.0;
    private double dy = 0.0;
    private double dz = 0.0;

    public double dxInA() {
        return dxInA;
    }

    public double dyInA() {
        return dyInA;
    }

    public double dzInA() {
        return dzInA;
    }

    public double dx() {
        return dx;
    }

    public double dy() {
        return dy;
    }

    public double dz() {
        return dz;
    }

    public void setdxInA(double dxInA) {
        this.dxInA = dxInA;
        this.dx = dxInA * PhysicalConstants.A;
    }

    public void setdyInA(double dyInA) {
        this.dyInA = dyInA;
        this.dy = dyInA * PhysicalConstants.A;
    }

    public void setdzInA(double dzInA) {
        this.dzInA = dzInA;
        this.dz = dzInA * PhysicalConstants.A;
    }

    public void setdx(double dx) {
        this.dx = dx;
        this.dxInA = dx / PhysicalConstants.A;
    }

    public void setdy(double dy) {
        this.dy = dy;
        this.dyInA = dy / PhysicalConstants.A;
    }

    public void setdz(double dz) {
        this.dz = dz;
        this.dzInA = dz / PhysicalConstants.A;
    }

    public void setdxdydzInA(double dxInA, double dyInA, double dzInA) {
        setdxInA(dxInA);
        setdyInA(dyInA);
        setdzInA(dzInA);
    }

    @Override
    public String toString(UnitSystem unit) {
        switch (unit) {
            case GSD:
                return formatString(1 / PhysicalConstants.A, "A");
            case SI:
            default: // SI
                return formatString(0.1 / PhysicalConstants.A, "nm");
        }
    }

    private String formatString(double factor, String unit_str) {
        DecimalFormat dform = new DecimalFormat("##0.00000");
        return "Displacement (" + unit_str + ") : " + StringUtils.leftPad(dform.format(dx * factor), 10) + StringUtils.leftPad(dform.format(dy * factor), 10) + StringUtils.leftPad(dform.format(dz * factor), 10);
    }
}
