package uk.ac.cam.cares.jps.assetinfo;

import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
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

    private List<Pair<String, String>> properties = new ArrayList<>();
    private List<Pair<String, String>> basicProperties = new ArrayList<>();
    private List<Pair<String, String>> locationProperties = new ArrayList<>();
    private List<Pair<String, String>> supplierProperties = new ArrayList<>();
    private List<Pair<String, String>> priceProperties = new ArrayList<>();
    private List<Pair<String, String>> docLineProperties = new ArrayList<>();
    private List<Pair<String, String>> otherProperties = new ArrayList<>();

    // todo: remove space and case when comparing/searching for the key?
    List<String> basicInfoOrder = new ArrayList<>(Arrays.asList("Reference Label", "Type", "Assigned to", "IRI", "Inventory ID", "Comment"));
    List<String> locationInfoOrder = new ArrayList<>(Arrays.asList("Located in", "Seat Location", "Stored in"));
    List<String> supplierInfoOrder = new ArrayList<>(Arrays.asList("Vendor", "Serial Number", "Model Number"));  // todo: incomplete list, need to check the FIA query file
    List<String> priceInfoOrder = new ArrayList<>(Arrays.asList("Price", "Unit"));
    List<String> docLineInfoOrder = new ArrayList<>(Arrays.asList("Quotation Number", "Purchase Request Number", "Purchase Order Number", "Invoice Number", "Delivery Order Number"));

    List<String> sectionTitles = new ArrayList<>(Arrays.asList("Basic", "Location", "Supplier", "Price", "Purchase"));

    public AssetInfoAdapter() { }

    public AssetInfoAdapter(AssetInfo assetInfo) {
        properties = convertMapToList(assetInfo.getProperties());

        buildAllPropertiesList(assetInfo);
    }

    public void updateProperties(AssetInfo assetInfo) {
        properties = convertMapToList(assetInfo.getProperties());

        buildAllPropertiesList(assetInfo);
        notifyDataSetChanged();
    }

    private <K, V> List<Pair<K, V>> convertMapToList(Map<K, V> map) {
        List<Pair<K, V>> result = new ArrayList<>();
        for (K key : map.keySet()) {
            result.add(new Pair<>(key, map.get(key)));
        }

        result.sort(Comparator.comparing(kvPair -> kvPair.first.toString()));
        return result;
    }

    private void buildAllPropertiesList(AssetInfo assetInfo) {
        basicProperties = getOrderedPropertiesList(assetInfo.getProperties(), basicInfoOrder);
        locationProperties = getOrderedPropertiesList(assetInfo.getProperties(), locationInfoOrder);
        supplierProperties = getOrderedPropertiesList(assetInfo.getProperties(), supplierInfoOrder);
        priceProperties = getOrderedPropertiesList(assetInfo.getProperties(), priceInfoOrder);
        docLineProperties = getOrderedPropertiesList(assetInfo.getProperties(), docLineInfoOrder);
        otherProperties = getOrderedPropertiesList(assetInfo.getProperties(), null);
    }

    private List<Pair<String, String>> getOrderedPropertiesList(Map<String, String> map, List<String> orderList) {
        List<Pair<String, String>> result = new ArrayList<>();
        if (orderList != null) {
            for (String key : orderList) {
                result.add(new Pair<>(key, map.get(key)));
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
                .inflate(R.layout.property_item_view, parent, false);

        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.getLabelView().setText(properties.get(position).first);
        holder.getContentView().setText(properties.get(position).second);
    }

    @Override
    public int getItemCount() {
        return properties.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        private final TextView label;
        private final TextView content;


        public ViewHolder(View view) {
            super(view);
            label = (TextView) view.findViewById(R.id.label);
            content = (TextView) view.findViewById(R.id.content);
        }

        public TextView getLabelView() {
            return label;
        }

        public TextView getContentView() {
            return content;
        }
    }

}
