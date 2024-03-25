package uk.ac.cam.cares.jps.network;

import android.content.Context;

import okhttp3.HttpUrl;

public class NetworkConfiguration {

    public static HttpUrl.Builder constructUrlBuilder(String path, Context context) {
        return constructTestUrlBuilder(path, context);
//        return constructPublicUrlBuilder(path, context);
    }

    public static HttpUrl.Builder constructTestUrlBuilder(String path, Context context) {
        HttpUrl.Builder builder = HttpUrl.get(context.getString(R.string.test_host)).newBuilder();
        return builder.addPathSegments(path);
    }

    public static HttpUrl.Builder constructPublicUrlBuilder(String path, Context context) {
        HttpUrl.Builder builder = HttpUrl.get(context.getString(R.string.host)).newBuilder();
        return builder.addPathSegments(path);
    }
}
