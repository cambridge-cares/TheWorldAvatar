package uk.ac.cam.cares.jps.addasset.view;

import android.content.Context;
import android.graphics.Color;
import android.text.Editable;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.TextWatcher;
import android.text.style.ForegroundColorSpan;
import android.util.AttributeSet;
import android.view.View;
import android.widget.EditText;
import android.widget.RelativeLayout;

import com.google.android.material.textfield.TextInputLayout;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.addasset.R;
import uk.ac.cam.cares.jps.addasset.model.AssetPropertyDataModel;

abstract public class PropertyBaseInputTextView extends RelativeLayout {
    private final Logger LOGGER = Logger.getLogger(PropertyBaseInputTextView.class);

    TextInputLayout inputLayout;
    EditText editText;
    String propertyName; // debug use

    public PropertyBaseInputTextView(Context context) {
        super(context);
    }

    public PropertyBaseInputTextView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    // Subclass of this class should have input_layout for InputLayout and edit_text for EditText type
    public PropertyBaseInputTextView(Context context, int layoutId, AssetPropertyDataModel property) {
        super(context);
        propertyName = property.getFieldName();

        View view = inflate(getContext(), layoutId, this);
        inputLayout = view.findViewById(R.id.input_layout);
        if (property.isRequired()) {
            inputLayout.setHint(getMandatoryHintText(property.getFieldName()));
        } else {
            inputLayout.setHint(property.getFieldName());
        }

        editText = inputLayout.findViewById(R.id.edit_text);
        editText.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence charSequence, int i, int i1, int i2) {;}

            @Override
            public void onTextChanged(CharSequence charSequence, int i, int i1, int i2) {;}

            @Override
            public void afterTextChanged(Editable editable) {
                property.setFieldValue(editable.toString());
                // todo: how to get iri from the editable? for searchable field, supposed to have iri and leave the fieldValue empty... or to have both, so the input mandatory check is easier?
            }
        });
    }

    private SpannableString getMandatoryHintText(String propertyFieldName) {
        SpannableString hint = new SpannableString(propertyFieldName + " *");
        hint.setSpan(new ForegroundColorSpan(Color.RED), hint.length() - 1, hint.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
        return hint;
    }
}
