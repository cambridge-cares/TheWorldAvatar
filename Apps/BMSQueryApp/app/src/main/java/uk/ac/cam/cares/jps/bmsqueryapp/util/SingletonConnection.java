package uk.ac.cam.cares.jps.bmsqueryapp.util;

import android.content.Context;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.toolbox.Volley;

public class SingletonConnection {
    private static SingletonConnection instance;
    private RequestQueue requestQueue;
    private static Context context;

    private SingletonConnection(Context context) {
        this.context = context;
        requestQueue = getRequestQueue();

    }

    public static synchronized SingletonConnection getInstance(Context context) {
        if (instance == null) {
            instance = new SingletonConnection(context);
        }
        return instance;
    }

    public RequestQueue getRequestQueue() {
        if (requestQueue == null) {
            // getApplicationContext() is key, it keeps you from leaking the
            // Activity or BroadcastReceiver if someone passes one in.
            requestQueue = Volley.newRequestQueue(context.getApplicationContext());
        }
        return requestQueue;
    }

    public <T> void addToRequestQueue(Request<T> req) {
        getRequestQueue().add(req);
    }
}
