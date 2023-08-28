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
import android.widget.RelativeLayout;

import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.addasset.model.AssetPropertyDataModel;
import uk.ac.cam.cares.jps.addasset.R;

public class PropertyInputTextView extends RelativeLayout {
    private final Logger LOGGER = Logger.getLogger(PropertyInputTextView.class);

    public PropertyInputTextView(Context context) {
        super(context);
        inflate(getContext(), R.layout.view_input_text_layout, this);
    }

    public PropertyInputTextView(Context context, AttributeSet attrs) {
        super(context, attrs);
        inflate(getContext(), R.layout.view_input_text_layout, this);
    }

    public PropertyInputTextView(Context context, AssetPropertyDataModel property) {
        super(context);
        View inputText = inflate(getContext(), R.layout.view_input_text_layout, this);
        TextInputLayout inputLayout = inputText.findViewById(R.id.textField);
        if (property.isRequired()) {
            inputLayout.setHint(getMandatoryHintText(property.getFieldName()));
        } else {
            inputLayout.setHint(property.getFieldName());
        }

        if (!property.isSearchable()) {
            inputLayout.setEndIconMode(TextInputLayout.END_ICON_NONE);
            inputLayout.setEndIconDrawable(null);
            inputLayout.setEndIconContentDescription("");
        } else {
            inputLayout.setEndIconOnClickListener(view -> {
                LOGGER.info("end icon clicked");
            });
        }

        TextInputEditText editText = inputLayout.findViewById(R.id.edit_text);
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
