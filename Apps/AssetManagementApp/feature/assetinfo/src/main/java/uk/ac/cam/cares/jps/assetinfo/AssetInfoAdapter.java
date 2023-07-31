package uk.ac.cam.cares.jps.assetinfo;

import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.data.AssetInfo;

public class AssetInfoAdapter extends RecyclerView.Adapter<AssetInfoAdapter.ViewHolder>{

    private List<Pair<String, String>> properties = new ArrayList<>();

    public AssetInfoAdapter() { }

    public AssetInfoAdapter(AssetInfo assetInfo) {
        properties = convertMapToList(assetInfo.getProperties());
    }

    public void updateProperties(AssetInfo assetInfo) {
        properties = convertMapToList(assetInfo.getProperties());
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
