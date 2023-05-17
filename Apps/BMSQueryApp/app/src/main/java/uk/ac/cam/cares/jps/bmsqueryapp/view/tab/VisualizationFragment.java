package uk.ac.cam.cares.jps.bmsqueryapp.view.tab;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.JavascriptInterface;
import android.webkit.WebResourceError;
import android.webkit.WebResourceRequest;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.bmsqueryapp.databinding.FragmentVisualizationBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.Constants;

public class VisualizationFragment extends Fragment {

    /**
     * A data class used to inject application data to the visualisation webpage
     */
    static class JsObject {
        public String equipmentIri;
        public JsObject(String equipmentIri) {
            this.equipmentIri = equipmentIri;
        }

        /**
         * This function is called in the visualisation webpage to get the user selected equipmentIri
         * @return equipmentIri
         */
        @JavascriptInterface
        public String getEquipmentIri() {
            return equipmentIri;
        }
    }


    private static final Logger LOGGER = LogManager.getLogger(VisualizationFragment.class);

    private String DTVFURI = Constants.HOST_PROD + ":1234";

    public int requestedStatus;
    private FragmentVisualizationBinding binding;

    private String equipmentIri;

    public VisualizationFragment(String equipmentIri) {
        super();
        this.equipmentIri = equipmentIri;
    }

    public void setPosition(int position) {
        requestedStatus = position;
    }

    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        binding = FragmentVisualizationBinding.inflate(inflater, container, false);
        BasicConfigurator.configure();
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        loadDTVF(equipmentIri);
    }

    public void loadDTVF(String equipmentIri) {
        WebView.setWebContentsDebuggingEnabled(true);

        WebView dtvfViz = binding.dtvfViz;
        dtvfViz.setWebViewClient(new WebViewClient() {

            @Override
            public void onPageFinished(WebView view, String url) {
                super.onPageFinished(view, url);
                LOGGER.info("page finished loading");
//                binding.progressBarWrapper.setVisibility(View.GONE);
            }

            @Override
            public void onReceivedError(WebView view, WebResourceRequest request, WebResourceError error) {
                super.onReceivedError(view, request, error);
                LOGGER.error(error);
            }
        });

        JsObject jsObject = new JsObject(equipmentIri);

        dtvfViz.getSettings().setJavaScriptEnabled(true);
        binding.dtvfViz.addJavascriptInterface(jsObject, "jsObject");
        binding.dtvfViz.loadUrl("file:///android_asset/visualisation/index.html");
        LOGGER.info("view created");

        dtvfViz.setVisibility(View.VISIBLE);

        WebSettings webSettings = binding.dtvfViz.getSettings();
        if (webSettings.getTextZoom() > 150) {
            webSettings.setTextZoom(150);
        }
    }


}
