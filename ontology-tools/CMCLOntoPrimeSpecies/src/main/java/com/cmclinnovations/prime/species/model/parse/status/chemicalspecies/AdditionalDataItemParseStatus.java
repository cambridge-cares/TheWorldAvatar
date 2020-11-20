package com.cmclinnovations.prime.species.model.parse.status.chemicalspecies;

public class AdditionalDataItemParseStatus {
	boolean additionalDataItem = false;

	public boolean isAdditionalDataItem() {
		return additionalDataItem;
	}

	public void setAdditionalDataItem(boolean additionalDataItem) {
		this.additionalDataItem = additionalDataItem;
	}

	boolean itemType = false;

	public boolean isItemType() {
		return itemType;
	}

	public void setItemType(boolean itemType) {
		this.itemType = itemType;
	}

	boolean description = false;

	public boolean isDescription() {
		return description;
	}

	public void setDescription(boolean description) {
		this.description = description;
	}

	boolean MIME = false;

	public boolean isMIME() {
		return MIME;
	}

	public void setMIME(boolean MIME) {
		this.MIME = MIME;
	}

}
