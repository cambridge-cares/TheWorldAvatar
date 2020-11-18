package com.cmclinnovations.ontokin.model.parse.status.ctml;
/**
 * This class contains getters and setters to flags that maintain
 * whether or not a CtmlComment metadata element or attribute has already been parsed.
 * 
 * @author msff2
 *
 */
public class CtmlCommentParseStatus {
	boolean comment = false;
	boolean material = false;
	public boolean isComment() {
		return comment;
	}
	public void setComment(boolean comment) {
		this.comment = comment;
	}
	public boolean isMaterial() {
		return material;
	}
	public void setMaterial(boolean material) {
		this.material = material;
	}
}
