package uk.ac.cam.cares.jps.bmsqueryapp.adapter.spinner;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.ArrayList;
import java.util.Collection;

import uk.ac.cam.cares.jps.bmsqueryapp.data.Instance;

public class InstanceAdapter extends BaseArrayAdapter<Instance>{
    private ArrayList<Instance> instances;
    public InstanceAdapter(Context context, String placeholderStr) {
        super(context);

        Instance placeholder = new Instance("Please select...");
        placeholder.setLabel(placeholderStr);
        this.placeholder = placeholder;

        instances = new ArrayList<>();
        instances.add(placeholder);
    }

    @Override
    public void add(@Nullable Instance object) {
        super.add(object);
        instances.add(object);
    }

    @Override
    public void addAll(@NonNull Collection<? extends Instance> collection) {
        super.addAll(collection);
        instances.addAll(collection);
    }

    @Nullable
    @Override
    public Instance getItem(int position) {
        return instances.get(position);
    }

    @Override
    public int getCount() {
        return instances.size();
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        TextView tv = (TextView) super.getView(position, convertView, parent);
        tv.setText(instances.get(position).getLabel());
        return tv;
    }

    @Override
    public View getDropDownView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        TextView tv = (TextView) super.getDropDownView(position, convertView, parent);
        tv.setText(instances.get(position).getLabel());
        return tv;
    }

    @Override
    public void clear() {
        super.clear();
        instances.clear();
        instances.add(placeholder);
    }
}
