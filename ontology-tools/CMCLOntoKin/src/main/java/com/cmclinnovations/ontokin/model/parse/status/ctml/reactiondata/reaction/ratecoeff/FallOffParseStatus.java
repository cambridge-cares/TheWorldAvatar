package com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff;

public class FallOffParseStatus {
	boolean fallOff = false;
	boolean type = false;
	boolean namedThirdBody = false;
	
	public boolean isFallOff() {
		return fallOff;
	}
	public void setFallOff(boolean fallOff) {
		this.fallOff = fallOff;
	}
	public boolean isType() {
		return type;
	}
	public void setType(boolean type) {
		this.type = type;
	}
	public boolean isNamedThirdBody() {
		return namedThirdBody;
	}
	public void setNamedThirdBody(boolean namedThirdBody) {
		this.namedThirdBody = namedThirdBody;
	}
}
