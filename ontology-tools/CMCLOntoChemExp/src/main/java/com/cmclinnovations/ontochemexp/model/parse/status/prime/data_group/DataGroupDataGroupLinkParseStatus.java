package com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group;

public class DataGroupDataGroupLinkParseStatus {
	boolean dataGroupLink = false;
	boolean dataGroupLinkDataGroupID =false;
	boolean dataGroupLinkDataPointID =false;
	
	public boolean isDataGroupLink() {
		return dataGroupLink;
	}
	
	public void setDataGroupLink(boolean dataGroupLink) {
		this.dataGroupLink = dataGroupLink;
	}
	
	public boolean isDataGroupLinkDataGroupID() {
		return dataGroupLinkDataGroupID;
	}
	
	public void setDataGroupLinkDataGroupID(boolean dataGroupLinkDataGroupID) {
		this.dataGroupLinkDataGroupID = dataGroupLinkDataGroupID;
	}
	
	public boolean isDataGroupLinkDataPointID() {
		return dataGroupLinkDataPointID;
	}
	
	public void setDataGroupLinkDataPointID(boolean dataGroupLinkDataPointID) {
		this.dataGroupLinkDataPointID = dataGroupLinkDataPointID;
	}
}
