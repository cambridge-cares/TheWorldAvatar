package uk.ac.cam.cares.jps.bmsqueryapp.adapter.list;

import android.text.Editable;
import android.text.TextWatcher;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;

import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.bmsqueryapp.R;
import uk.ac.cam.cares.jps.bmsqueryapp.data.attribtue.EditableAttribute;

public class EditableAttributesAdapter extends RecyclerView.Adapter<EditableAttributesAdapter.EditableAttributeInputView> {

    static class EditableAttributeInputView extends RecyclerView.ViewHolder {

        TextInputEditText textInputEditText;
        TextInputLayout layout;
        TextView unitTv;
        EditTextListener listener;

        public EditableAttributeInputView(@NonNull View itemView, EditTextListener listener) {
            super(itemView);
            textInputEditText = itemView.findViewById(R.id.textinput_editext);
            textInputEditText.addTextChangedListener(listener);
            unitTv =  itemView.findViewById(R.id.unit_tv);
            layout = itemView.findViewById(R.id.textinput_layout);
            this.listener = listener;
        }

        void enableTextWatcher() {
            textInputEditText.addTextChangedListener(listener);
        }

        void disableTextWatcher() {
            textInputEditText.removeTextChangedListener(listener);
        }
    }

    class EditTextListener implements TextWatcher {
        // the text watcher also serves the purpose to collect all user input
        int position;

        void updatePosition(int position) {
            this.position = position;
        }

        @Override
        public void beforeTextChanged(CharSequence charSequence, int i, int i1, int i2) { }

        @Override
        public void onTextChanged(CharSequence charSequence, int i, int i1, int i2) {
            editableAttributes.get(position).setValue(charSequence.toString());
        }

        @Override
        public void afterTextChanged(Editable editable) { }
    }

    List<EditableAttribute> editableAttributes;

    public EditableAttributesAdapter (List<EditableAttribute> editableAttributes) {
        this.editableAttributes = editableAttributes;
    }

    @NonNull
    @Override
    public EditableAttributeInputView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.edit_attribute_textinput_layout, parent, false);
        return new EditableAttributeInputView(view, new EditTextListener());
    }

    @Override
    public void onBindViewHolder(@NonNull EditableAttributeInputView holder, int position) {
        EditableAttribute current = editableAttributes.get(position);

        TextInputEditText textInputEditText = holder.textInputEditText;
        textInputEditText.setInputType(current.getInputType());
        holder.listener.updatePosition(position);
        textInputEditText.setText(editableAttributes.get(position).getValue());


        TextInputLayout layout = holder.layout;
        layout.setHint(current.getName());

        TextView unitTv =  holder.unitTv;
        unitTv.setText(current.getUnit());
    }

    @Override
    public int getItemCount() {
        return editableAttributes.size();
    }

    @Override
    public void onViewAttachedToWindow(@NonNull EditableAttributeInputView holder) {
        holder.enableTextWatcher();
    }

    @Override
    public void onViewDetachedFromWindow(@NonNull EditableAttributeInputView holder) {
        holder.disableTextWatcher();
    }
}
