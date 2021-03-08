package uk.ac.cam.ceb.como.math.unit;

import uk.ac.cam.ceb.como.math.unit.Property;
import uk.ac.cam.ceb.como.math.unit.UnitSystem;

/**
 * todo: property should not have setter method
 *
 * @author Weerapong Phadungsukanan
 */
public abstract class CompositeProperty implements Property {

    /**
     * String description of the property object.
     */
    @Override
    public String toString() {
        return toString(UnitSystem.SI);
    }
}
