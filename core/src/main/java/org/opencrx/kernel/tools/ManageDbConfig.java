/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Manage DB Config
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
 * * Neither the name of the openCRX team nor the names of the contributors
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
package org.opencrx.kernel.tools;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

public class ManageDbConfig {

	public static class LinkedProperties extends Properties {
	
	    private final HashSet<Object> keys = new LinkedHashSet<Object>();

	    public LinkedProperties() {
	    }

	    public Iterable<Object> orderedKeys() {
	        return Collections.list(keys());
	    }

	    public Enumeration<Object> keys() {
	        return Collections.<Object>enumeration(keys);
	    }

	    public Object put(Object key, Object value) {
	        keys.add(key);
	        return super.put(key, value);
	    }		
	}

	public static void main(
		String[] args
	) throws IOException {
		InputStream propertiesFile = new FileInputStream("./src/jar/opencrx-resources.jar/META-INF/Kernel.properties");
		LinkedProperties config = new LinkedProperties();
		config.load(propertiesFile);
		Map<String,Map<String,String>> dbObjectTypes = new TreeMap<String,Map<String,String>>();
		// Phase 1: add types
		for(Object objKey: config.orderedKeys()) {
			String key = (String)objKey;
			String value = (String)config.getProperty(key);
			if(key.startsWith("PERSISTENCE/typeName[")) {
				int index = Integer.parseInt(key.substring(key.indexOf("[") + 1, key.indexOf("]")));
				Map<String,String> dbObjectType = dbObjectTypes.get(value);
				if(dbObjectType != null) {
					System.out.println("ERROR: duplicate typeName " + key);
					break;
				}
				dbObjectTypes.put(
					value,
					dbObjectType = new LinkedHashMap<String,String>()
				);
				dbObjectType.put("index", Integer.toString(index));
			}
		}
		// Phase 2: add properties
		for(Object objKey: config.orderedKeys()) {
			String key = (String)objKey;
			String value = (String)config.getProperty(key);
			if(
				key.startsWith("PERSISTENCE/type[") ||
				key.startsWith("PERSISTENCE/typeName[") ||
				key.startsWith("PERSISTENCE/dbObject[") ||
				key.startsWith("PERSISTENCE/dbObject2[") ||
				key.startsWith("PERSISTENCE/pathNormalizeLevel[") ||
				key.startsWith("PERSISTENCE/dbObjectFormat[") ||
				key.startsWith("PERSISTENCE/dbObjectForQuery[") ||
				key.startsWith("PERSISTENCE/dbObjectForQuery2[") ||
				key.startsWith("PERSISTENCE/dbObjectsForQueryJoinColumn[") ||
				key.startsWith("PERSISTENCE/joinTable[") ||
				key.startsWith("PERSISTENCE/joinColumnEnd1[") ||				
				key.startsWith("PERSISTENCE/joinColumnEnd2[")				
		    ) {	
				int index = Integer.parseInt(key.substring(key.indexOf("[") + 1, key.indexOf("]")));
				Map<String,String> dbObjectType = null;
				for(Map<String,String> candidate: dbObjectTypes.values()) {
					if(Integer.parseInt(candidate.get("index")) == index) {
						dbObjectType = candidate;
						break;
					}
				}
				if(dbObjectType == null) {
					System.out.println("ERROR: no matching type for given key " + key);
					break;
				}
				dbObjectType.put(key, value);
			}
		}
		// Phase 3: print out dbObjectTypes
		int count = 0;
		for(Map<String,String> dbObjectType: dbObjectTypes.values()) {
			System.out.println("#");
			for(Map.Entry<String,String> property: dbObjectType.entrySet()) {
				String key = property.getKey();
				if(!key.equals("index")) {
					key = key.substring(0, key.indexOf("["));
					System.out.println(key + "[" + count + "]=" + property.getValue());
				}
			}
			count++;
		}
	}
	
}
