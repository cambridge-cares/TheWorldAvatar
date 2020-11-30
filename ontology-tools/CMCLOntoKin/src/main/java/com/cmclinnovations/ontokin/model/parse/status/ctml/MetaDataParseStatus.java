package com.cmclinnovations.ontokin.model.parse.status.ctml;

/**
 * This class contains getters and setters to flags that maintain
 * whether or not a CTML metadata element or attribute has already been parsed.
 * 
 * @author msff2
 *
 */
public class MetaDataParseStatus {
	boolean ctml = false;
	boolean ctmlCmclVersion = false;
	boolean ctmlCommit = false;
	
	public boolean isCtml() {
		return ctml;
	}
	public void setCtml(boolean ctml) {
		this.ctml = ctml;
	}
	public boolean isCtmlCmclVersion() {
		return ctmlCmclVersion;
	}
	public void setCtmlCmclVersion(boolean ctmlCmclVersion) {
		this.ctmlCmclVersion = ctmlCmclVersion;
	}
	public boolean isCtmlCommit() {
		return ctmlCommit;
	}
	public void setCtmlCommit(boolean ctmlCommit) {
		this.ctmlCommit = ctmlCommit;
	}
}
