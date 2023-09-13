package uk.ac.cam.cares.jps.setting;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.LinearLayout;

import androidx.annotation.NonNull;
import androidx.core.util.Pair;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModel;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.checkbox.MaterialCheckBox;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class SettingAssetInfoAdapter extends RecyclerView.Adapter<SettingAssetInfoAdapter.ViewHolder>{
    private Map<String, List<String>> checkboxStatusBySections = new LinkedHashMap<>();
    private Context context;
    private SettingViewModel viewModel;
    private LifecycleOwner lifecycleOwner;

    public SettingAssetInfoAdapter(Context context, SettingViewModel viewModel, LifecycleOwner lifecycleOwner) {
        this.context = context;
        this.viewModel = viewModel;
        this.lifecycleOwner = lifecycleOwner;
        buildCheckboxStatusSections();
    }

    private void buildCheckboxStatusSections() {
        checkboxStatusBySections.put(BASIC_SECTION_TITLE, basicInfoOrder);
        checkboxStatusBySections.put(LOCATION_SECTION_TITLE, locationInfoOrder);
        checkboxStatusBySections.put(SUPPLIER_SECTION_TITLE, supplierInfoOrder);
        checkboxStatusBySections.put(PURCHASE_SECTION_TITLE, docLineInfoOrder);
        checkboxStatusBySections.put(ITEM_SECTION_TITLE, itemInfoOrder);
        checkboxStatusBySections.put(OTHERS_SECTION_TITLE, Arrays.asList(SPEC_SHEET_SECTION_TITLE, MANUAL_SECTION_TITLE));
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.view_asset_info_setting_section, parent, false);

        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        String currentSectionName = new ArrayList<>(checkboxStatusBySections.keySet()).get(position);
        holder.sectionParentCb.setText(currentSectionName);
        viewModel.getSettingByKey(currentSectionName).observe(lifecycleOwner, integer -> holder.sectionParentCb.setCheckedState(integer));
        ((MaterialCheckBox) holder.sectionParentCb).addOnCheckedStateChangedListener(getParentCheckboxListener(currentSectionName, holder));

        holder.linearLayout.removeAllViews();
        for (String itemKey : checkboxStatusBySections.get(currentSectionName)) {
            MaterialCheckBox checkBox = new MaterialCheckBox(holder.linearLayout.getContext());
            checkBox.setText(itemKey);
            viewModel.getSettingByKey(itemKey).observe(lifecycleOwner, checkBox::setCheckedState);
            checkBox.addOnCheckedStateChangedListener(getChildCheckboxListener(itemKey, holder));

            holder.linearLayout.addView(checkBox);
        }
    }

    private MaterialCheckBox.OnCheckedStateChangedListener getParentCheckboxListener(String settingKey, ViewHolder holder) {
        return (checkBox, state) -> {
            // update all children checkbox
            if (state == MaterialCheckBox.STATE_CHECKED) {
                for (int i = 0; i < holder.linearLayout.getChildCount(); i++) {
                    ((MaterialCheckBox) holder.linearLayout.getChildAt(i)).setCheckedState(MaterialCheckBox.STATE_CHECKED);
                }
            } else if (state == MaterialCheckBox.STATE_UNCHECKED) {
                for (int i = 0; i < holder.linearLayout.getChildCount(); i++) {
                    ((MaterialCheckBox) holder.linearLayout.getChildAt(i)).setCheckedState(MaterialCheckBox.STATE_UNCHECKED);
                }
            }
        };
    }

    private MaterialCheckBox.OnCheckedStateChangedListener getChildCheckboxListener(String settingKey, ViewHolder holder) {
        return (checkBox, state) -> {
            viewModel.getSettingByKey(settingKey).setValue(state);
            // update parent checkbox
            holder.sectionParentCb.setCheckedState(getParentCheckboxState(holder));
        };
    }

    private int getParentCheckboxState(ViewHolder holder) {
        int checkedChild = 0;
        for (int i = 0; i < holder.linearLayout.getChildCount(); i ++) {
            if (((MaterialCheckBox) holder.linearLayout.getChildAt(i)).getCheckedState() == MaterialCheckBox.STATE_CHECKED) {
                checkedChild ++;
            }
        }
        if (checkedChild == holder.linearLayout.getChildCount()) {
            return MaterialCheckBox.STATE_CHECKED;
        } else if (checkedChild == 0) {
            return MaterialCheckBox.STATE_UNCHECKED;
        }
        else {
             return MaterialCheckBox.STATE_INDETERMINATE;
        }
    }

    @Override
    public int getItemCount() {
        return checkboxStatusBySections.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        MaterialCheckBox sectionParentCb;
        LinearLayout linearLayout;

        public ViewHolder(@NonNull View itemView) {
            super(itemView);

            sectionParentCb = itemView.findViewById(R.id.section_parent_cb);
            linearLayout = itemView.findViewById(R.id.section_container);
        }

        public void setSectionParentCbText(String title) {
            this.sectionParentCb.setText(title);
        }
    }
}
