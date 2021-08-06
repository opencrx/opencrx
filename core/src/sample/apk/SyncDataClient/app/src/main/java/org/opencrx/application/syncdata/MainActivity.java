package org.opencrx.application.syncdata;

import androidx.appcompat.app.AppCompatActivity;

import android.content.ContentValues;
import android.content.Context;
import android.database.sqlite.SQLiteDatabase;
import android.os.Bundle;
import android.text.method.ScrollingMovementMethod;
import android.view.View;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.android.volley.AuthFailureError;
import com.android.volley.DefaultRetryPolicy;
import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.JsonObjectRequest;
import com.android.volley.toolbox.Volley;

import org.json.JSONObject;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
    }

    private void transferTo(
            InputStream source,
            OutputStream target
    ) throws IOException {
        byte[] buf = new byte[8192];
        int length;
        while ((length = source.read(buf)) > 0) {
            target.write(buf, 0, length);
        }
    }

    public void doWork(
            String applicationName,
            File databaseFile,
            TextView console
    ) {
        try {
            if (databaseFile.exists()) {
                // Add a new activity and set all existing to is_dirty=true
                {
                    SQLiteDatabase db = SQLiteDatabase.openDatabase(databaseFile.toString(), null, SQLiteDatabase.OPEN_READWRITE);
                    db.beginTransaction();
                    // Add new activity
                    {
                        ContentValues values = new ContentValues();
                        values.put("name", applicationName + " @ " + new Date());
                        values.put("description", "Incident created by SyncDataTask @ " + new Date());
                        values.put("scheduled_start", System.currentTimeMillis());
                        values.put("scheduled_end", System.currentTimeMillis() + 3600000L);
                        values.put("due_by", System.currentTimeMillis() + 3600000L);
                        values.put("priority", Short.valueOf((short) 0));
                        values.put("is_dirty", true);
                        db.insert("activity", null, values);
                    }
                    // Set existing to is_dirty=true
                    {
                        ContentValues values = new ContentValues();
                        values.put("is_dirty", true);
                        values.put("description", "Incident updated by SyncDataTask @ " + new Date());
                        db.update("activity", values, "CAST(activity_number as decimal) % 4 = 0", null);
                    }
                    db.setTransactionSuccessful();
                    db.endTransaction();
                    db.close();
                }
            }
        } catch(Exception e) {
            console.append(e.getMessage() + "\n");
        }
    }

    public void syncData(
            String url,
            String databaseName,
            File databaseFile,
            String syncProfileName,
            TextView console,
            ProgressBar progressBar
    ) {
        try {
            String fileContentBase64 = null;
            if (databaseFile.exists()) {
                ByteArrayOutputStream fileContent = new ByteArrayOutputStream();
                transferTo(new FileInputStream(databaseFile), fileContent);
                fileContentBase64 = new String(Base64.getEncoder().encode(fileContent.toByteArray()), StandardCharsets.ISO_8859_1);
            }
            JSONObject params = new JSONObject(
                "{\n" +
                    "  \"org.opencrx.kernel.workflow1.RunImportExportParams\": {\n" +
                    (fileContentBase64 == null ? "" : "\"param1\": \"" + fileContentBase64 + "\",\n") +
                    (databaseName == null ? "" : "\"param2\": \"" + databaseName + "\",\n") +
                    "    \"param3\": \"application/vnd.sqlite3\",\n" +
                    "    \"param4\": \"" + syncProfileName + "\"\n" +
                    "  }\n" +
                "}"
            );
            RequestQueue queue = Volley.newRequestQueue(this);
            JsonObjectRequest jsonObjectRequest = new JsonObjectRequest(
                    Request.Method.POST,
                    url,
                    params,
                    new Response.Listener<JSONObject>() {
                        @Override
                        public void onResponse(JSONObject response) {
                            try {
                                if (response.getString("file") != null) {
                                    transferTo(
                                            new ByteArrayInputStream(Base64.getDecoder().decode(response.getString("file"))),
                                            new FileOutputStream(databaseFile)
                                    );
                                    console.append("status: " + response.getString("status") + "\n");
                                    console.append("statusMessage:\n" + response.getString("statusMessage") + "\n");
                                }
                            } catch(Exception e) {
                                console.append(e.getMessage() + "\n");
                            }
                            progressBar.setVisibility(ProgressBar.GONE);
                        }
                    },
                    new Response.ErrorListener() {
                        @Override
                        public void onErrorResponse(VolleyError error) {
                            console.append(error.getMessage() + "\n");
                        }
                    }
            ) {
                @Override
                public Map<String, String> getHeaders() throws AuthFailureError {
                    Map<String,String> headers = new HashMap<String,String>();
                    String credentials = USERNAME+":"+PASSWORD;
                    String auth = "Basic " + Base64.getEncoder().encodeToString(credentials.getBytes());
                    headers.put("Authorization", auth);
                    return headers;
                }
            };
            jsonObjectRequest.setRetryPolicy(
                new DefaultRetryPolicy(
                    (int) TimeUnit.SECONDS.toMillis(120),
                    0, // disable retries
                    DefaultRetryPolicy.DEFAULT_BACKOFF_MULT
                )
            );
            queue.add(jsonObjectRequest);
        } catch(Exception e) {
            console.append(e.getMessage() + "\n");
        }
    }

    public void syncData(View view) {
        Context context = getApplicationContext();
        String url = SYNC_URL;
        String databaseName = DATABASE_NAME;
        String syncProfileName = SYNC_PROFILE_NAME;
        File databaseFile = context.getDatabasePath(DATABASE_NAME);
        String applicationName = getString(R.string.app_name);
        TextView console = (TextView) findViewById(R.id.textView);
        console.setText("");
        console.setMovementMethod(new ScrollingMovementMethod());
        this.doWork(
                applicationName,
                databaseFile,
                console
        );
        ProgressBar progressBar = findViewById(R.id.progressBar);
        progressBar.setVisibility(ProgressBar.VISIBLE);
        this.syncData(
                url,
                databaseName,
                databaseFile,
                syncProfileName,
                console,
                progressBar
        );
    }

    public final String USERNAME = "admin-Standard";
    public final String PASSWORD = "admin-Standard";
    public static final String SYNC_URL = "http://192.168.1.160:8080/opencrx-rest-CRX/org.opencrx.kernel.workflow1/provider/CRX/segment/Standard/wfProcess/SyncDataTask/runImport";
    public static final String DATABASE_NAME = "test-activities.db";
    public static final String SYNC_PROFILE_NAME = "org.opencrx.kernel.tasks/test-activities";

}
