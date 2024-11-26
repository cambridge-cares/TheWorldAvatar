package uk.ac.cam.cares.jps.datastore;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.SharedPreferences;

import androidx.datastore.preferences.core.MutablePreferences;
import androidx.datastore.preferences.core.Preferences;
import androidx.datastore.preferences.rxjava2.RxPreferenceDataStoreBuilder;
import androidx.datastore.rxjava2.RxDataStore;
import androidx.test.platform.app.InstrumentationRegistry;
import androidx.test.ext.junit.runners.AndroidJUnit4;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.reactivestreams.Subscriber;
import org.reactivestreams.Subscription;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.reactivex.Flowable;
import io.reactivex.Single;
import io.reactivex.disposables.Disposable;
import io.reactivex.observers.DisposableSingleObserver;
import kotlinx.coroutines.flow.Flow;


/**
 * Instrumented test, which will execute on an Android device.
 *
 * @see <a href="http://d.android.com/tools/testing">Testing documentation</a>
 */
@RunWith(AndroidJUnit4.class)
public class ExampleInstrumentedTest {
    @Test
    public void useAppContext() {
        // Context of the app under test.
        Context appContext = InstrumentationRegistry.getInstrumentation().getTargetContext();
        assertEquals("uk.ac.cam.cares.jps.datastore.test", appContext.getPackageName());

        RxDataStore<Preferences> dataStore =
                new RxPreferenceDataStoreBuilder(appContext, /*name=*/ "types").build();

        // data
        List<String> iris = Arrays.asList("iri1", "iri2", "iri3");
        List<String> labels = Arrays.asList("Monitor", "Coffee Machine", "Projector");

        dataStore.updateDataAsync(prefsIn -> {
            MutablePreferences mutablePreferences = prefsIn.toMutablePreferences();
            for (int i = 0; i < iris.size(); i++) {
                mutablePreferences.set(new Preferences.Key<>(iris.get(i)), labels.get(i));
            }
            return Single.just(mutablePreferences);
        });

        Flowable<Map<Preferences.Key<?>, Object>> exampleCounterFlow =
                dataStore.data().map(Preferences::asMap);
        exampleCounterFlow.subscribeWith(new Subscriber<Object>() {
            @SuppressLint("CheckResult")
            @Override
            public void onSubscribe(Subscription s) {

            }

            @Override
            public void onNext(Object o) {
                System.out.println("call back");
                System.out.println(o.toString());
                Map<Preferences.Key<String>, String> m = (Map<Preferences.Key<String>, String>)o;
                int i = 0;
                for (Preferences.Key<String> key : m.keySet()) {
                    assertEquals(iris.get(i), key);
                    assertEquals(labels.get(i), m.get(key));
                    i++;
                }

            }

            @Override
            public void onError(Throwable t) {
                t.printStackTrace();
            }

            @Override
            public void onComplete() {
                System.out.println("completed");
            }

        });

    }

    @Test
    public void testPreference() {
        Context appContext = InstrumentationRegistry.getInstrumentation().getTargetContext();
        SharedPreferences pref = appContext.getSharedPreferences("testPref", Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = pref.edit();
        editor.putString("test1", "test2");
        editor.apply();
    }

    @Test
    public void clearDataStore() {
        Context appContext = InstrumentationRegistry.getInstrumentation().getTargetContext();
        RxDataStore<Preferences> dataStore =
                new RxPreferenceDataStoreBuilder(appContext, /*name=*/ "types").build();
        dataStore.updateDataAsync(prefsIn -> {
            MutablePreferences mutablePreferences = prefsIn.toMutablePreferences();
            mutablePreferences.clear();
            return Single.just(mutablePreferences);
        });
    }

    @SuppressLint("CheckResult")
    @Test
    public void readDataStore() {
        // Context of the app under test.
        Context appContext = InstrumentationRegistry.getInstrumentation().getTargetContext();
//        assertEquals("uk.ac.cam.cares.jps.datastore.test", appContext.getPackageName());

        System.out.println("test started");

        RxDataStore<Preferences> dataStore =
                new RxPreferenceDataStoreBuilder(appContext, /*name=*/ "types").build();

        List<String> iris = Arrays.asList("iri1", "iri2", "iri3");
        List<String> labels = Arrays.asList("label1", "label2", "label3");


        Flowable<Map<Preferences.Key<?>, Object>> exampleCounterFlow =
                dataStore.data().map(Preferences::asMap);
        exampleCounterFlow.subscribeWith(new Subscriber<Object>() {
            @Override
            public void onSubscribe(Subscription s) {

            }

            @Override
            public void onNext(Object o) {
                System.out.println("call back");
                System.out.println(o.toString());
                Map<Preferences.Key<String>, String> m = (Map<Preferences.Key<String>, String>)o;
                int i = 0;
                for (Preferences.Key<String> key : m.keySet()) {
                    assertEquals(iris.get(i), key);
                    assertEquals(labels.get(i), m.get(key));
                    i++;
                }

            }

            @Override
            public void onError(Throwable t) {
                t.printStackTrace();
            }

            @Override
            public void onComplete() {

            }

        });

    }

    @Test
    public void testFlowable() {
//        Map<String, String> m = new HashMap<>();
//        m.put("item1", "1");
//        m.put("item2", "2");
//        m.put("item3", "3");
//        Flowable<Map<String, String>> flowable = Flowable.just(m);


        Flowable<Integer> flowable = Flowable.just(1, 2, 3, 4, 5);

        // Use the first operator to get the first emitted item or a default value
//        Single<Integer> firstItem = flowable.first(0); // Default value is 0 if no item is emitted

        // Subscribe to the Single to get the result
//        Disposable disposable = firstItem.subscribeWith(new DisposableSingleObserver<Integer>() {
//            @Override
//            public void onSuccess(Integer integer) {
//                System.out.println("First item: " + integer);
//            }
//
//            @Override
//            public void onError(Throwable e) {
//                e.printStackTrace();
//            }
//        });

//        Disposable disposable = flowable.subscribe(map -> {
//            for (String k : map.keySet()) {
//                System.out.println(k + ": " + map.get(k));
//            }
//        });

        flowable.subscribe(new Subscriber<Integer>() {
            @Override
            public void onSubscribe(Subscription s) {
                System.out.println("Subscriber 1: onSubscribe");
                s.request(3);
            }

            @Override
            public void onNext(Integer integer) {
                System.out.println("Subscriber 1: " + integer);
            }

            @Override
            public void onError(Throwable t) {

            }

            @Override
            public void onComplete() {

            }
        });

        try {
            Thread.sleep(2000);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

        flowable.subscribe(new Subscriber<Integer>() {
            @Override
            public void onSubscribe(Subscription s) {
                System.out.println("Subscriber 2: onSubscribe");
                s.request(2);
            }

            @Override
            public void onNext(Integer integer) {
                System.out.println("Subscriber 2: " + integer);
            }

            @Override
            public void onError(Throwable t) {

            }

            @Override
            public void onComplete() {
            }
        });
    }
}