/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: BpiAddress
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2013, CRIXP Corp., Switzerland
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
package org.opencrx.application.bpi.datatype;

import java.lang.reflect.Type;
import java.util.List;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

/**
 * BpiAddress
 *
 */
public class BpiAddress extends BpiObject {

	/**
	 * BpiAddressDeserializer
	 *
	 */
	public static class BpiAddressDeserializer implements JsonDeserializer<BpiAddress> {
		
		/**
		 * Constructor.
		 * 
		 * @param gsonBuilder
		 */
		public BpiAddressDeserializer(
			GsonBuilder gsonBuilder
		) {
			this.gsonBuilder = gsonBuilder;
		}
		
		/* (non-Javadoc)
		 * @see com.google.gson.JsonDeserializer#deserialize(com.google.gson.JsonElement, java.lang.reflect.Type, com.google.gson.JsonDeserializationContext)
		 */
		public BpiAddress deserialize(
			JsonElement json, 
			Type typeOfT, 
			JsonDeserializationContext context
		) throws JsonParseException {
			Gson gson = this.gsonBuilder.create();
			if(json instanceof JsonObject) {
				JsonObject jsonObject = (JsonObject)json;
				if(jsonObject.has("emailAddress")) {
					return gson.fromJson(json, BpiEMailAddress.class);
				} else if(jsonObject.has("phoneNumberFull")) {
					return gson.fromJson(json, BpiPhoneNumber.class);				
				} else {
					return gson.fromJson(json, BpiPostalAddress.class);				
				}
			} else {
				return null;
			}
		}
		
		private final GsonBuilder gsonBuilder;
	}
	
	/**
	 * @return the usage
	 */
	public List<Short> getUsage() {
		return usage;
	}
	/**
	 * @param usage the usage to set
	 */
	public void setUsage(List<Short> usage) {
		this.usage = usage;
	}
	/**
	 * @return the isMain
	 */
	public boolean isMain() {
		return isMain;
	}
	/**
	 * @param isMain the isMain to set
	 */
	public void setMain(boolean isMain) {
		this.isMain = isMain;
	}
	
	private List<Short> usage;
	private boolean isMain;
}
