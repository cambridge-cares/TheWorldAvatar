package org.cam.ceb.como.nist.webbook.thermochem;
/**
 * A model defined to encode the geometry of a chemical element.
 * 
 * @author msff2
 *
 */
public class NISTElementGeometry {
	String element;
	String x;
	String y;
	String z;
	public String getElement() {
		return element;
	}
	public void setElement(String element) {
		this.element = element;
	}
	public String getX() {
		return x;
	}
	public void setX(String x) {
		this.x = x;
	}
	public String getY() {
		return y;
	}
	public void setY(String y) {
		this.y = y;
	}
	public String getZ() {
		return z;
	}
	public void setZ(String z) {
		this.z = z;
	}
}
