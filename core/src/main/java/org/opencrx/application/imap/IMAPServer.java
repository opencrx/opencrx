/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: IMAPServer
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2009, CRIXP Corp., Switzerland
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
package org.opencrx.application.imap;

import java.net.Socket;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.mail.MessagingException;

import org.opencrx.application.adapter.AbstractServer;
import org.opencrx.application.adapter.AbstractSession;
import org.opencrx.kernel.activity1.jmi1.ActivityCategory;
import org.opencrx.kernel.activity1.jmi1.ActivityMilestone;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.generic.SecurityKeys;
import org.openmdx.base.naming.Path;

/**
 * IMAPServer
 *
 */
public class IMAPServer extends AbstractServer {
    
	/**
	 * Constructor.
	 * @param pmf
	 * @param providerName
	 * @param bindAddress
	 * @param portNumber
	 * @param sslKeystoreFile
	 * @param sslKeystoreType
	 * @param sslKeystorePass
	 * @param sslKeyPass
	 * @param isDebug
	 * @param delayOnStartup
	 */
	protected IMAPServer(
	    PersistenceManagerFactory pmf,
	    String providerName,
	    String bindAddress,
	    int portNumber,
	    String sslKeystoreFile,
	    String sslKeystoreType,
	    String sslKeystorePass,
	    String sslKeyPass,
	    String sslTruststoreFile,
	    String sslTruststorePass,
	    String sslTruststoreType,
	    Boolean sslNeedClientAuth,	    
	    boolean isDebug,
	    int delayOnStartup
	) {
		super(
			"IMAPServer",
			pmf,
			providerName,
		    bindAddress,
		    portNumber,
		    sslKeystoreFile,
		    sslKeystoreType,
		    sslKeystorePass,
		    sslKeyPass,
		    sslTruststoreFile,
		    sslTruststorePass,
		    sslTruststoreType,
		    sslNeedClientAuth,
			isDebug,
			delayOnStartup
		);
	}
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.adapter.AbstractServer#newSession(java.net.Socket, org.opencrx.application.adapter.AbstractServer)
	 */
	@Override
    public AbstractSession newSession(
    	Socket socket, 
    	AbstractServer server
    ) {
		return new IMAPSession(
			socket,
			server
		);
    }
	
    /**
     * Encode folder name.
     * @param name
     * @return
     */
    protected String encodeFolderName(
        String name
    ) {
        // Slash is qualified name separator. Do not allow in folder names
        name = name.replace("/", ".");
        StringBuilder encodedName = new StringBuilder();
        for(int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if((c <= 127) && (c != '&')) {
                encodedName.append(c);
            }
            else if(c == '&') {
            	encodedName.append("&-");
            }
            else {
                String base64EncodedChar = org.openmdx.base.text.conversion.Base64.encode(new byte[]{(byte)(c / 256), (byte)(c % 256)});
                encodedName.append("&" + base64EncodedChar.substring(0, base64EncodedChar.length()-1) + "-");
            }
        }
        return encodedName.toString();      
    }
    
    /**
     * Return all folders which the user is allowed to subscribe.
     * @param segmentName
     * @return
     * @throws MessagingException
     */
    public Map<String,String> getAvailableFolders(
    	String segmentName    	
    ) throws MessagingException {     
    	if(
    		(System.currentTimeMillis() > this.refreshFoldersAt) || 
    		(this.availableFolders.get(segmentName) == null)
    	) {
    		PersistenceManager pm = AbstractSession.newPersistenceManager(
    			this.getPersistenceManagerFactory(), 
    			SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName + "@" + segmentName
    		);
    		try {
	            Map<String,String> folders = new HashMap<String,String>();    		
		        String providerName = this.getProviderName();
		        org.opencrx.kernel.activity1.jmi1.Segment activitySegment = 
		            (org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
		                new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", providerName, "segment", segmentName)
		            );
		        Collection<ActivityTracker> trackers = activitySegment.getActivityTracker();
		        for(org.opencrx.kernel.activity1.jmi1.ActivityGroup group: trackers) {
		            String groupUri = "/" + providerName + "/" + segmentName + "/tracker/";
		            folders.put(
		                "INBOX" + groupUri + this.encodeFolderName(group.getName()),
		                groupUri + group.getName()
		            );
		        }
		        Collection<ActivityMilestone> milestones = activitySegment.getActivityMilestone();
		        for(org.opencrx.kernel.activity1.jmi1.ActivityGroup group: milestones) {
		            String groupUri = "/" + providerName + "/" + segmentName + "/milestone/";
		            folders.put(
		                "INBOX" + groupUri + this.encodeFolderName(group.getName()),
		                groupUri + group.getName()
		            );
		        }
		        Collection<ActivityCategory> categories = activitySegment.getActivityCategory();
		        for(org.opencrx.kernel.activity1.jmi1.ActivityGroup group: categories) {
		            String groupUri = "/" + providerName + "/" + segmentName + "/category/";
		            folders.put(
		                "INBOX" + groupUri + this.encodeFolderName(group.getName()),
		                groupUri + group.getName()
		            );
		        }
		        this.availableFolders.put(
		        	segmentName, 
		        	folders
		        );
		        this.refreshFoldersAt = System.currentTimeMillis() + FOLDER_REFRESH_PERIOD_MILLIS;
    		}
    		finally {
    			if(pm != null) {
    				pm.close();
    			}
    		}
    	}
        return this.availableFolders.get(segmentName);
    }
  	
    //-----------------------------------------------------------------------
	// Members
    //-----------------------------------------------------------------------
    private static final long FOLDER_REFRESH_PERIOD_MILLIS = 300000L;
    
    /**
     * Cache available folders per segment
     */
    protected Map<String,Map<String,String>> availableFolders = new ConcurrentHashMap<String,Map<String,String>>();
    protected long refreshFoldersAt = System.currentTimeMillis();
    
}
