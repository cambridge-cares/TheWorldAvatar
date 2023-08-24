package uk.ac.cam.cares.jps.assetinfo;

import static uk.ac.cam.cares.jps.assetinfo.AssetInfoConstant.*;

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
import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.data.AssetInfo;

public class AssetInfoAdapter extends RecyclerView.Adapter<AssetInfoAdapter.ViewHolder>{
    private List<Pair<String, String>> basicProperties = new ArrayList<>();
    private List<Pair<String, String>> locationProperties = new ArrayList<>();
    private List<Pair<String, String>> supplierProperties = new ArrayList<>();
    private List<Pair<String, String>> priceProperties = new ArrayList<>();
    private List<Pair<String, String>> docLineProperties = new ArrayList<>();
    private List<Pair<String, String>> otherProperties = new ArrayList<>();

    // todo: remove space and case when comparing/searching for the key?
    List<String> basicInfoOrder = new ArrayList<>(Arrays.asList(REFERENCE_LABEL, TYPE, ASSIGNED_TO, IRI, INVENTORY_ID));
    List<String> locationInfoOrder = new ArrayList<>(Arrays.asList(LOCATED_IN, SEAT_LOCATION, STORED_IN));
    List<String> supplierInfoOrder = new ArrayList<>(Arrays.asList(VENDOR, MANUFACTURER, MANUFACTURE_URL, SERIAL_NUMBER, MODEL_NUMBER));
    List<String> priceInfoOrder = new ArrayList<>(Arrays.asList(PURCHASE_PRICE));
    List<String> docLineInfoOrder = new ArrayList<>(Arrays.asList(SERVICE_CATEGORY_CODE, SERVICE_CATEGORY_DESCRIPTION, QUOTATION_NUMBER, PURCHASE_REQUEST_NUMBER, PURCHASE_ORDER_NUMBER, INVOICE_NUMBER, DELIVERY_ORDER_NUMBER));

    List<String> sectionTitles = new ArrayList<>(Arrays.asList("Basic", "Location", "Supplier", "Price", "Purchase", "Others"));
    List<List<Pair<String, String>>> sectionContents = new ArrayList<>(Arrays.asList(basicProperties, locationProperties, supplierProperties, priceProperties, docLineProperties, otherProperties));

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
        basicProperties = getOrderedPropertiesList(assetInfo.getProperties(), basicInfoOrder);
        locationProperties = getOrderedPropertiesList(assetInfo.getProperties(), locationInfoOrder);
        supplierProperties = getOrderedPropertiesList(assetInfo.getProperties(), supplierInfoOrder);
        priceProperties = getOrderedPropertiesList(assetInfo.getProperties(), priceInfoOrder);
        docLineProperties = getOrderedPropertiesList(assetInfo.getProperties(), docLineInfoOrder);
        otherProperties = getOrderedPropertiesList(assetInfo.getProperties(), null);

        sectionContents = new ArrayList<>(Arrays.asList(basicProperties, locationProperties, supplierProperties, priceProperties, docLineProperties, otherProperties));
    }

    private List<Pair<String, String>> getOrderedPropertiesList(Map<String, String> map, List<String> orderList) {
        List<Pair<String, String>> result = new ArrayList<>();
        if (orderList != null) {
            for (String key : orderList) {
                if (map.containsKey(key) && !map.get(key).isEmpty()) {
                    result.add(new Pair<>(key, map.get(key)));
                }
            }
            return result;
        }

        // return list of other keys
        List<String> allKeys = new ArrayList<>();
        allKeys.addAll(basicInfoOrder);
        allKeys.addAll(locationInfoOrder);
        allKeys.addAll(supplierInfoOrder);
        allKeys.addAll(priceInfoOrder);
        allKeys.addAll(docLineInfoOrder);

        for (String key : map.keySet()) {
            if (! allKeys.contains(key)) {
                result.add(new Pair<>(key, map.get(key)));
            }
        }

        result.sort(Comparator.comparing(kvPair -> kvPair.first));
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
        holder.getLabelView().setText(sectionTitles.get(position));

        LinearLayout linearLayout = holder.getLinearLayout();
        for (Pair<String, String> content : sectionContents.get(position)) {
            PropertyItemView propertyItemView = new PropertyItemView(context);
            propertyItemView.initView(content.first, content.second);
            linearLayout.addView(propertyItemView);
        }

    }

    @Override
    public int getItemCount() {
        return sectionTitles.size();
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
