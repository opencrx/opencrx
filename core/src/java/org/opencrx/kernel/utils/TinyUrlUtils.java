/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TinyUrlUtils
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2010, CRIXP Corp., Switzerland
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 * 
 * * Neither the name of CRIXP Corp. nor the names of the contributors
 * to openCRX may be used to endorse or promote products derived
 * from this software without specific prior written permission
 * 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * ------------------
 * 
 * This product includes software developed by the Apache Software
 * Foundation (http://www.apache.org/).
 * 
 * This product includes software developed by contributors to
 * openMDX (http://www.openmdx.org/)
 */
package org.opencrx.kernel.utils;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class TinyUrlUtils {

	//-----------------------------------------------------------------------
    public static String getTinyUrl(
    	String fullUrl
    ) {
    	String tinyUrl = tinyUrls.get(fullUrl);
    	if(tinyUrl == null) {
	    	HttpURLConnection conn = null;
	    	try {
	    		conn = (HttpURLConnection)new URL(
	    			PREFIX + "api-create.php?url=" + fullUrl
	    		).openConnection();
	    		conn.setDoInput(true);
	    		conn.setDoOutput(true);
	    		conn.setRequestMethod("GET");
	    		conn.connect();
	    		int responseCode = conn.getResponseCode();
	    		if(responseCode == HttpURLConnection.HTTP_OK) {
	                BufferedReader reader = new BufferedReader(
	                	new InputStreamReader(
	                    	conn.getInputStream(), 
	                    	"UTF-8"
	                    )
	                );
	                StringBuffer response = new StringBuffer();
	                String line;
	                while (null != (line = reader.readLine())) {
	                    response.append(line).append("\n");
	                }
	    			tinyUrl = response.toString();
	    			if(tinyUrl != null && tinyUrl.indexOf("\n") > 0) {
	    				tinyUrl = tinyUrl.substring(0, tinyUrl.indexOf("\n"));
	    			}
	    			if(tinyUrl != null) {
	    				tinyUrls.putIfAbsent(
	    					fullUrl, 
	    					tinyUrl
	    				);
	    			}
	    		}
	    	}
	    	catch(Exception e) {}
	    	finally {
	    		if(conn != null) {
	    			conn.disconnect();
	    		}
	    	}
    	}
    	return tinyUrl;
    }

	//-------------------------------------------------------------------------
    // Members
	//-------------------------------------------------------------------------
    public static final String PREFIX = "http://tinyurl.com/";
    
    private static ConcurrentMap<String,String> tinyUrls = new ConcurrentHashMap<String,String>();
    
}
