package uk.ac.cam.cares.jps.assetinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import android.content.Context;
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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import uk.ac.cam.cares.jps.data.AssetInfo;

public class AssetInfoAdapter extends RecyclerView.Adapter<AssetInfoAdapter.ViewHolder>{
    // todo: remove space and case when comparing/searching for the key?
    Map<String, List<Pair<String, String>>> propertiesBySections = new LinkedHashMap<>();

    Context context;
    public AssetInfoAdapter() { }

    public AssetInfoAdapter(AssetInfo assetInfo) {
        buildAllPropertiesList(assetInfo);
    }

    public void updateProperties(AssetInfo assetInfo) {
        buildAllPropertiesList(assetInfo);
        notifyDataSetChanged();
    }

    private void buildAllPropertiesList(AssetInfo assetInfo) {
        Map<String, String> map = (Map<String, String>) assetInfo.getProperties().clone();
        propertiesBySections.put(BASIC, getOrderedPropertiesList(map, basicInfoOrder));
        propertiesBySections.put(LOCATION, getOrderedPropertiesList(map, locationInfoOrder));
        propertiesBySections.put(SUPPLIER, getOrderedPropertiesList(map, supplierInfoOrder));
        propertiesBySections.put(PURCHASE, getItemAndDocLineOrderedPropertiesList(map));
        propertiesBySections.put(OTHERS, getOrderedPropertiesList(map, null));

    }

    private List<Pair<String, String>> getOrderedPropertiesList(Map<String, String> map, List<String> orderList) {
        List<Pair<String, String>> result = new ArrayList<>();
        if (orderList != null) {
            for (String key : orderList) {
                if (map.containsKey(key) && !map.get(key).isEmpty()) {
                    result.add(new Pair<>(key, map.get(key)));
                    map.remove(key);
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
            if (! allKeys.contains(key)) {
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
            if (!map.containsKey(key) || (map.containsKey(key) && map.get(key).isEmpty())) {
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
