package com.cmclinnovations.prime.species.model.parse.status.chemicalspecies;

public class ContentParseStatus {
	boolean content = false;

	public boolean isContent() {
		return content;
	}

	public void setContent(boolean content) {
		this.content = content;
	}

	boolean source = false;

	public boolean isSource() {
		return source;
	}

	public void setSource(boolean source) {
		this.source = source;
	}

	boolean copyrighted = false;

	public boolean isCopyrighted() {
		return copyrighted;
	}

	public void setCopyrighted(boolean copyrighted) {
		this.copyrighted = copyrighted;
	}

	boolean bibliography = false;

	public boolean isBibliography() {
		return bibliography;
	}

	public void setBibliography(boolean bibliography) {
		this.bibliography = bibliography;
	}
}
