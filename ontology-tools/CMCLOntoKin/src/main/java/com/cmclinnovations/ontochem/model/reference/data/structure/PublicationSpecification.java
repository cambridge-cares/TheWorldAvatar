package com.cmclinnovations.ontochem.model.reference.data.structure;
/**
 * Represents the model of a publication specification with respect to a</br> 
 * reference.
 * 
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
public class PublicationSpecification {
	private int pageStart;
	private int pageEnd;
	public int getPageStart() {
		return pageStart;
	}
	public void setPageStart(int pageStart) {
		this.pageStart = pageStart;
	}
	public int getPageEnd() {
		return pageEnd;
	}
	public void setPageEnd(int pageEnd) {
		this.pageEnd = pageEnd;
	}
}
