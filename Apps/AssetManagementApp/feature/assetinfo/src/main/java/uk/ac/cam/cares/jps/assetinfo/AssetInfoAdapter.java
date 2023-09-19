package uk.ac.cam.cares.jps.assetinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.net.Uri;
import android.text.SpannableString;
import android.text.style.ForegroundColorSpan;
import android.text.style.UnderlineSpan;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import uk.ac.cam.cares.jps.data.AssetInfo;

public class AssetInfoAdapter extends RecyclerView.Adapter<AssetInfoAdapter.ViewHolder>{
    // todo: remove space and case when comparing/searching for the key?
    Map<String, List<Pair<String, String>>> propertiesBySections = new LinkedHashMap<>();
    boolean skipEmptyField = true;

    Context context;
    public AssetInfoAdapter() { }

    public AssetInfoAdapter(AssetInfo assetInfo, boolean skipEmptyField) {
        this.skipEmptyField = skipEmptyField;
        buildAllPropertiesList(assetInfo);
    }

    public void updateProperties(AssetInfo assetInfo) {
        buildAllPropertiesList(assetInfo);
        notifyDataSetChanged();
    }

    private void buildAllPropertiesList(AssetInfo assetInfo) {
        Map<String, String> map = (Map<String, String>) assetInfo.getProperties().clone();

        List<Pair<String, String>> basicPropertyLists = getOrderedPropertiesList(map, basicInfoOrder);
        if (basicPropertyLists.size() != 0) {
            propertiesBySections.put(BASIC_SECTION_TITLE, basicPropertyLists);
        }

        List<Pair<String, String>> locationPropertyLists = getOrderedPropertiesList(map, locationInfoOrder);
        if (locationPropertyLists.size() != 0) {
            propertiesBySections.put(LOCATION_SECTION_TITLE, locationPropertyLists);
        }

        List<Pair<String, String>> supplierPropertyLists = getOrderedPropertiesList(map, supplierInfoOrder);
        if (supplierPropertyLists.size() != 0) {
            propertiesBySections.put(SUPPLIER_SECTION_TITLE, supplierPropertyLists);
        }

        // merge item info and docline info to purchase section
        List<Pair<String, String>> purchasePropertyLists = getItemAndDocLineOrderedPropertiesList(map);
        if (purchasePropertyLists.size() != 0) {
            propertiesBySections.put(PURCHASE_SECTION_TITLE, purchasePropertyLists);
        }

        List<Pair<String, String>> maintenancePropertyLists = getOrderedPropertiesList(map, maintenanceInfoOrder);
        if (maintenancePropertyLists.size() != 0) {
            propertiesBySections.put(MAINTENANCE_SECTION_TITLE, maintenancePropertyLists);
        }

        List<Pair<String, String>> otherPropertyLists = getOrderedPropertiesList(map, null);
        if (otherPropertyLists.size() != 0) {
            propertiesBySections.put(OTHERS_SECTION_TITLE, otherPropertyLists);
        }

    }

    private List<Pair<String, String>> getOrderedPropertiesList(Map<String, String> map, List<String> orderList) {
        List<Pair<String, String>> result = new ArrayList<>();
        if (orderList != null) {
            for (String key : orderList) {
                if (map.containsKey(key)) {
                    if (skipEmptyField && !map.get(key).isEmpty()) {
                        result.add(new Pair<>(key, map.get(key)));
                        map.remove(key);
                    } else if (!skipEmptyField) {
                        result.add(new Pair<>(key, map.get(key)));
                        map.remove(key);
                    }
                }
            }
            return result;
        }

        // return list of other keys
        List<String> allKeys = new ArrayList<>();
        allKeys.addAll(basicInfoOrder);
        allKeys.addAll(locationInfoOrder);
        allKeys.addAll(supplierInfoOrder);
        allKeys.addAll(itemInfoOrder);
        allKeys.addAll(docLineInfoOrder);

        for (String key : map.keySet()) {
            if (key.equals(HAS_TIME_SERIES)) {
                continue;
            }
            if (!allKeys.contains(key)) {
                result.add(new Pair<>(key, map.get(key)));
            }
        }

        result.sort(Comparator.comparing(kvPair -> kvPair.first));
        return result;
    }

    // handle multiple attachment and comment
    private List<Pair<String, String>> getItemAndDocLineOrderedPropertiesList(Map<String, String> map) {
        List<Pair<String, String>> result = new ArrayList<>();
        List<String> itemAndDocLineKeys = new ArrayList<>();
        itemAndDocLineKeys.addAll(itemInfoOrder);
        itemAndDocLineKeys.addAll(docLineInfoOrder);

        for (String key : itemAndDocLineKeys) {
            if (!map.containsKey(key)) {
                continue;
            }

            if (skipEmptyField && map.get(key).isEmpty()) {
                continue;
            }

            // item related
            if (itemInfoOrder.contains(key)) {
                result.add(new Pair<>(key, map.get(key)));
                map.remove(key);
                continue;
            }

            // docLine
            String type = key.split(" ")[0];
            result.add(new Pair<>(key, map.get(key)));
            List<String> attachmentKeys = map.keySet().stream().filter(k -> k.contains(type) && k.contains("Attachment")).collect(Collectors.toList());
            for (String attachmentKey : attachmentKeys) {
                int leftBracketInd = attachmentKey.indexOf("(");
                int rightBracketInd = attachmentKey.indexOf(")");
                String fileName = attachmentKey.substring(leftBracketInd + 1, rightBracketInd);
                String commentKey = "Comments for " + fileName;

                result.add(new Pair<>(attachmentKey, map.get(attachmentKey)));
                map.remove(attachmentKey);

                if (!map.get(commentKey).isEmpty()) {
                    result.add(new Pair<>(commentKey, map.get(commentKey)));
                    map.remove(commentKey);
                }
            }
        }

        return result;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.property_section_view, parent, false);
        context = view.getContext();

        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        String currentSectionName = new ArrayList<>(propertiesBySections.keySet()).get(position);
        holder.getLabelView().setText(currentSectionName);

        LinearLayout linearLayout = holder.getLinearLayout();
        linearLayout.removeAllViews();
        for (Pair<String, String> content : propertiesBySections.getOrDefault(currentSectionName, new ArrayList<>())) {
            PropertyItemView propertyItemView = new PropertyItemView(context);
            propertyItemView.initView(content.first, content.second);

            // browser intent
            if (content.first.contains("Attachment")
                    || Arrays.asList(MANUAL_SECTION_TITLE, SPEC_SHEET_SECTION_TITLE).contains(content.first)
                    || content.first.equals(MANUFACTURE_URL)) {
                String value = content.second;
                SpannableString spannableString = new SpannableString(value);

                UnderlineSpan underlineSpan = new UnderlineSpan();
                spannableString.setSpan(underlineSpan, 0, value.length(), 0);

                ForegroundColorSpan blueColorSpan = new ForegroundColorSpan(Color.BLUE);
                spannableString.setSpan(blueColorSpan, 0, value.length(), 0);
                propertyItemView.setValueTv(spannableString);

                Intent pdfIntent = new Intent(Intent.ACTION_VIEW, Uri.parse(value));
                propertyItemView.setOnClickListener(view -> context.startActivity(pdfIntent));
            }

            linearLayout.addView(propertyItemView);
        }

    }

    @Override
    public int getItemCount() {
        return propertiesBySections.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        private final TextView label;
        private final LinearLayout linearLayout;

        public ViewHolder(View view) {
            super(view);

            label = view.findViewById(R.id.section_label);
            linearLayout = view.findViewById(R.id.linear_layout);
        }

        public TextView getLabelView() {
            return label;
        }

        public LinearLayout getLinearLayout() {
            return linearLayout;
        }
    }

}
