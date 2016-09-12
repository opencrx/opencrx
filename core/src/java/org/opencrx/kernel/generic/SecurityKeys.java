/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Description: openCRX application plugin
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2005, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.generic;

public class SecurityKeys {

    public static final String ADMIN_PRINCIPAL = "admin";
    public static final String ROOT_PRINCIPAL = "admin-Root";
    public static final String ADMIN_USER_ID = "admin";
    public static final String LOADER_PRINCIPAL = "loader";
    public static final String LOADER_USER_ID = "loader";
    public static final String USER_GROUP_UNSPECIFIED = "Unspecified";
    public static final String USER_GROUP_ADMINISTRATORS = "Administrators";
    public static final String USER_GROUP_USERS = "Users";
    public static final String USER_GROUP_UNASSIGNED = "Unassigned";
    public static final String USER_GROUP_PUBLIC = "Public";
    public static final String ROOT_ADMINISTRATORS_GROUP = "Root:Administrators";
    public static final String ROOT_ROOTS_GROUP = "Root:Roots";
    public static final String PRINCIPAL_GROUP_ADMINISTRATORS = "Administrators";
    public static final String USER_SUFFIX = "User";
    public static final String GROUP_SUFFIX = "Group";
    
    // Principal types
    public final static String PRINCIPAL_TYPE_GROUP = "org:opencrx:security:realm1:PrincipalGroup";
    public final static String PRINCIPAL_TYPE_USER = "org:opencrx:security:realm1:User";
    public final static String PRINCIPAL_TYPE_PRINCIPAL = "org:opencrx:security:realm1:Principal";
            
    // System Properties
    public static final String ENABLE_SECURITY_PROPERTY = "org.opencrx.security.enable";
    public static final String REALM_REFRESH_RATE_MILLIS = "org.opencrx.security.realmRefreshRateMillis";
    public static final String ID_SEPARATOR_PROPERTY = "org.opencrx.SecurityKeys.ID_SEPARATOR";
    public static final String ID_SEPARATOR = System.getProperty(ID_SEPARATOR_PROPERTY) == null ? "-" : System.getProperty(SecurityKeys.ID_SEPARATOR_PROPERTY);

    // Encodings
    public static final String PASSWORD_ENCODING_SCHEME = "Base64:";

    // Actions for permissions
    public enum Action {
    	
    	DELETE("delete"),
    	UPDATE("update"),
    	READ("read"),
    	RUN_AS("runAs");
    	
    	private final String name;
    	
    	private Action(
    		String name
    	) {
    		this.name = name;
    	}
    	
    	public String getName(
    	) {
    		return this.name;
    	}
    
    	public String toString(
    	) {
    		return this.name;
    	}
    	
    }

    // Access levels
    public static final short ACCESS_LEVEL_NA = 0;
    public static final short ACCESS_LEVEL_PRIVATE = 1;
    public static final short ACCESS_LEVEL_BASIC = 2;
    public static final short ACCESS_LEVEL_DEEP = 3;
    public static final short ACCESS_LEVEL_GLOBAL = 4;
    
}

//--- End of File -----------------------------------------------------------
