package uk.ac.cam.cares.jps.bmsqueryapp.view.tab;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.JavascriptInterface;
import android.webkit.ValueCallback;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.bmsqueryapp.R;
import uk.ac.cam.cares.jps.bmsqueryapp.databinding.FragmentVisualizationBinding;

public class VisualizationFragment extends Fragment {

    /**
     * A data class used to inject application data to the visualisation webpage
     */
    static class JsObject {
        public String equipmentIri;
        public final String hostAddr;
        public final String fiaPath;
        private VisualizationFragment fragment;

        public JsObject(String equipmentIri, VisualizationFragment fragment, Context context) {
            this.equipmentIri = equipmentIri;
            this.fragment = fragment;

            this.hostAddr = context.getString(R.string.host);
            this.fiaPath = context.getString(R.string.feature_info_agent_get_path);
        }

        /**
         * This function is called in the visualisation webpage to get the user selected equipmentIri
         * @return equipmentIri
         */
        @JavascriptInterface
        public String getEquipmentIri() {
            return equipmentIri;
        }

        @JavascriptInterface
        public void notifyChartReady() {
            fragment.finishLoading();
        }

        @JavascriptInterface
        public String getHostAddr() {
            return hostAddr;
        }

        @JavascriptInterface
        public String getFiaPath() {
            return fiaPath;
        }
    }


    private static final Logger LOGGER = LogManager.getLogger(VisualizationFragment.class);

    public int requestedStatus;
    private FragmentVisualizationBinding binding;

    private String equipmentIri;
    private WebViewClient webViewClient;
    private ValueCallback<String> reloadCallback;

    public VisualizationFragment(String equipmentIri, WebViewClient webViewClient, ValueCallback<String> reloadCallback) {
        super();
        this.equipmentIri = equipmentIri;
        this.webViewClient = webViewClient;
        this.reloadCallback = reloadCallback;
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

        binding.dtvfViz.setWebViewClient(webViewClient);
        loadDTVF();
    }

    public void loadDTVF() {
        WebView.setWebContentsDebuggingEnabled(true);

        WebView dtvfViz = binding.dtvfViz;

        JsObject jsObject = new JsObject(equipmentIri, this, requireContext());

        dtvfViz.getSettings().setJavaScriptEnabled(true);
        binding.dtvfViz.addJavascriptInterface(jsObject, "jsObject");
        binding.dtvfViz.loadUrl("file:///android_asset/visualisation/index.html");
        LOGGER.info("view created");

        WebSettings webSettings = binding.dtvfViz.getSettings();
        if (webSettings.getTextZoom() > 150) {
            webSettings.setTextZoom(150);
        }
    }

    public void refreshDTVF() {
        binding.dtvfViz.evaluateJavascript("refreshChart();", reloadCallback);
    }

    public void finishLoading() {
        requireActivity().runOnUiThread(() -> binding.progressBarWrapper.setVisibility(View.GONE));
        requireActivity().runOnUiThread(() -> binding.dtvfViz.setVisibility(View.VISIBLE));
    }

}
