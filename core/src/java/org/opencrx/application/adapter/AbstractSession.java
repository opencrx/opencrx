/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: openCRX application plugin
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2007, CRIXP Corp., Switzerland
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
package org.opencrx.application.adapter;

import java.net.Socket;

import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;

import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.security.authentication1.jmi1.Password;

/**
 * AbstractSession
 *
 */
public abstract class AbstractSession implements Runnable {

    public AbstractSession(
        Socket client, 
        AbstractServer server
    ) {
        this.socket = client;
        this.server = server;
    }

    /**
     * Stop session.
     */
    public void stop(
    ) {
    	if(this.socket != null) {
    		// Closing the socket stops the thread
    		try {
    			this.socket.close();
    			this.socket = null;
    		} catch(Exception e) {}    		
    	}
    }

    /**
     * Get persistence manager for user.
     * @param pmf
     * @param username
     * @return
     */
    public static PersistenceManager newPersistenceManager(
    	PersistenceManagerFactory pmf,
    	String username
    ) {
        String principalName = username.substring(0, username.indexOf("@"));            
        return pmf.getPersistenceManager(
            principalName, 
            null
        );                                	
    }
    
    /**
     * Validate user and password.
     * @param username
     * @param password
     * @return
     */
    protected boolean login(
        String username,
        String password
    ) {
        try {
            if(username.indexOf("@") > 0) {
                this.segmentName = username.substring(username.indexOf("@") + 1);
                this.username = username;
                String principalName = username.substring(0, username.indexOf("@"));            
                PersistenceManager rootPm = this.server.getPersistenceManagerFactory().getPersistenceManager(
                    SecurityKeys.ROOT_PRINCIPAL,
                    null
                );                
                org.opencrx.security.realm1.jmi1.Principal principal = 
                    (org.opencrx.security.realm1.jmi1.Principal)rootPm.getObjectById(
                        new Path("xri://@openmdx*org.openmdx.security.realm1").getDescendant("provider", this.server.getProviderName(), "segment", "Root", "realm", "Default", "principal", principalName)
                    );
                if(principal != null) {
                    org.openmdx.security.realm1.jmi1.Credential credential = principal.getCredential();                
                    if(credential instanceof Password) {
                        boolean success = Utils.getPasswordDigest(password, PASSWORD_ENCODING_ALGORITHM).equals(
                            "{" + PASSWORD_ENCODING_ALGORITHM + "}" + ((Password)credential).getPassword()
                        );
                        return success;
                    }
                }
                rootPm.close();
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
        }
        return false;
    }

    /**
     * Session logout.
     */
    protected void logout(
    ) {
    }
    
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------    
    public static final String PASSWORD_ENCODING_ALGORITHM = "MD5";
    
    protected final AbstractServer server;
    protected Socket socket;
    protected String username = null;
    protected String segmentName = null;
    
}
