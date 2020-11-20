package com.cmclinnovations.ontochemexp.model.parse.status.prime;

public class AdditionalDataItemParseStatus {
	boolean additionalDataItem = false;
	boolean MIME = false;
	boolean itemType = false;
	boolean description = false;

	public boolean isAdditionalDataItem() {
		return additionalDataItem;
	}

	public void setAdditionalDataItem(boolean additionalDataItem) {
		this.additionalDataItem = additionalDataItem;
	}

	public boolean isMIME() {
		return MIME;
	}

	public void setMIME(boolean MIME) {
		this.MIME = MIME;
	}

	public boolean isItemType() {
		return itemType;
	}

	public void setItemType(boolean itemType) {
		this.itemType = itemType;
	}

	public boolean isDescription() {
		return description;
	}

	public void setDescription(boolean description) {
		this.description = description;
	}
}
