package uk.ac.cam.cares.jps.bmsqueryapp.adapter.list;


import uk.ac.cam.cares.jps.bmsqueryapp.data.Equipment;

public interface OnEquipmentClickedListener {
    void onItemClicked(EquipmentAdapter.EquipmentViewHolder item, Equipment equipment);
}
