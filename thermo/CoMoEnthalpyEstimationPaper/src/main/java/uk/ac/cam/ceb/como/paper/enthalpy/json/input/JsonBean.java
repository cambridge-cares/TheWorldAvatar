package uk.ac.cam.ceb.como.paper.enthalpy.json.input;

import java.util.LinkedList;

public class JsonBean {	
	
	LinkedList<SpeciesBean> referenceSpecies;	
	
	String srcCompoundsRef;
	String srcRefPool;
	String srcTargetPool;
	String destRList;
	String tempFolder;
	String reactionType;
	String inputZipFile;
	String whichProcessToRun;
	int ctrRuns;
	int ctrRes;
	int ctrRadicals;	
	boolean isRefAndTargetSetSame;
	
	
	
	public boolean isRefAndTargetSetSame() {
		return isRefAndTargetSetSame;
	}
	public void setRefAndTargetSetSame(boolean isRefAndTargetSetSame) {
		this.isRefAndTargetSetSame = isRefAndTargetSetSame;
	}
	
	public int getCtrRuns() {
		return ctrRuns;
	}
	public void setCtrRuns(int ctrRuns) {
		this.ctrRuns = ctrRuns;
	}
	public int getCtrRes() {
		return ctrRes;
	}
	public void setCtrRes(int ctrRes) {
		this.ctrRes = ctrRes;
	}
	public int getCtrRadicals() {
		return ctrRadicals;
	}
	public void setCtrRadicals(int ctrRadicals) {
		this.ctrRadicals = ctrRadicals;
	}
	public LinkedList<SpeciesBean> getReferenceSpecies() {
		return referenceSpecies;
	}
	public void setReferenceSpecies(LinkedList<SpeciesBean> referenceSpecies) {
		this.referenceSpecies = referenceSpecies;
	}
	public String getSrcCompoundsRef() {
		return srcCompoundsRef;
	}
	public void setSrcCompoundsRef(String srcCompoundsRef) {
		this.srcCompoundsRef = srcCompoundsRef;
	}
	public String getSrcRefPool() {
		return srcRefPool;
	}
	public void setSrcRefPool(String srcRefPool) {
		this.srcRefPool = srcRefPool;
	}
	public String getSrcTargetPool() {
		return srcTargetPool;
	}
	public void setSrcTargetPool(String srcTargetPool) {
		this.srcTargetPool = srcTargetPool;
	}
	public String getDestRList() {
		return destRList;
	}
	public void setDestRList(String destRList) {
		this.destRList = destRList;
	}
	public String getTempFolder() {
		return tempFolder;
	}
	public void setTempFolder(String tempFolder) {
		this.tempFolder = tempFolder;
	}
	public String getReactionType() {
		return reactionType;
	}
	public void setReactionType(String reactionType) {
		this.reactionType = reactionType;
	}
	
	public String getInputZipFile() {
		return inputZipFile;
	}
	public void setInputZipFile(String inputZipFile) {
		this.inputZipFile = inputZipFile;
	}
	public String getWhichProcessToRun() {
		return whichProcessToRun;
	}
	public void setWhichProcessToRun(String whichProcessToRun) {
		this.whichProcessToRun = whichProcessToRun;
	}	
}