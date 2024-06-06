package uk.ac.cam.cares.jps.bmsqueryapp.utils;

import android.content.Context;

import okhttp3.HttpUrl;
import uk.ac.cam.cares.jps.bmsqueryapp.R;

public class Constants {

    public static String[] statusArrayTemp = {"Graph", "Edit"};

    public static HttpUrl.Builder constructUrlBuilder(String path, Context context) {
        HttpUrl.Builder builder = HttpUrl.get(context.getString(R.string.host)).newBuilder();
        return builder.addPathSegments(path);
    }

    public static HttpUrl.Builder constructUrlBuilder(String host, String path) {
        HttpUrl.Builder builder = HttpUrl.get(host).newBuilder();
        return builder.addPathSegments(path);
    }

}
