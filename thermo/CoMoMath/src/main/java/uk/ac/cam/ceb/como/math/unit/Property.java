package uk.ac.cam.ceb.como.math.unit;

import uk.ac.cam.ceb.como.math.unit.UnitSystem;

/**
 * todo: property should not have setter method
 *
 * @author Weerapong Phadungsukanan
 */
public interface Property {

    /**
     * get a string description for the property object using given unit system
     *
     * @param unit unit system
     * @return a string description for the property object
     */
    String toString(UnitSystem unit);
}
