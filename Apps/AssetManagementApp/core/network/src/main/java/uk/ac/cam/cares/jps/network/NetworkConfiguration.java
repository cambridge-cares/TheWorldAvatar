package uk.ac.cam.cares.jps.network;

import okhttp3.HttpUrl;

public class NetworkConfiguration {
    public static final String HOST_DEV = "192.168.1.16";
    public static final String HOST_LOCALHOST = "10.0.2.2";

    public static final String HOST_LAB_WIFI = "192.168.51.103";
    public static final String HOST_OFFICE = "10.25.188.58";
    public static final String HOST_PROD = "137.132.22.165";
    public static final String HOST = HOST_DEV;

    public static HttpUrl.Builder constructUrlBuilder(String path) {
//        return constructUrlBuilder(HOST, 3838, path);
        return constructPublicUrlBuilder(path);
    }

    public static HttpUrl.Builder constructUrlBuilder(String host, int port, String path) {
        HttpUrl.Builder builder = new HttpUrl.Builder().scheme("http").host(host).port(port);
        return builder.addPathSegments(path);
    }

    public static HttpUrl.Builder constructPublicUrlBuilder(String path) {
        HttpUrl.Builder builder = new HttpUrl.Builder().scheme("https").host("www.theworldavatar.com").port(1010).addPathSegments("careslab");
        return builder.addPathSegments(path);
    }
}
