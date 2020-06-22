/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: MailImporterConfig
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2012, CRIXP Corp., Switzerland
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
package org.opencrx.application.mail.importer;

import java.util.Collection;

import javax.jdo.PersistenceManager;

import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.kernel.log.SysLog;

/**
 * MailImporterConfig
 *
 */
public class MailImporterConfig {

    /**
     * Get the MailImporter config from component configuration. Create default
     * properties on-the-fly

     * @param pm
     * @param providerName
     * @param segmentName
     */
    public MailImporterConfig(
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        String mailServiceName = null;
        String mailbox = null;
        Boolean deleteImportedMessages = null;     
        try {
            org.opencrx.kernel.admin1.jmi1.Segment adminSegment = 
                (org.opencrx.kernel.admin1.jmi1.Segment)pm.getObjectById(
                    new Path("xri://@openmdx*org.opencrx.kernel.admin1").getDescendant("provider", providerName, "segment", "Root")
                );
            org.opencrx.kernel.admin1.jmi1.ComponentConfiguration componentConfiguration = null;
            // Get component configuration
            try {
                componentConfiguration = adminSegment.getConfiguration(
                    COMPONENT_CONFIGURATION_ID
                );
            } catch(Exception uncaught) {}
            String optionPrefix = providerName + "." + segmentName + ".";
            if(componentConfiguration == null) {
                componentConfiguration = pm.newInstance(org.opencrx.kernel.admin1.jmi1.ComponentConfiguration.class);
                componentConfiguration.setName(COMPONENT_CONFIGURATION_ID);
                pm.currentTransaction().begin();
                adminSegment.addConfiguration(
                    COMPONENT_CONFIGURATION_ID,
                    componentConfiguration
                );
                // Default for mail service name
                org.opencrx.kernel.base.jmi1.StringProperty sp = pm.newInstance(org.opencrx.kernel.base.jmi1.StringProperty.class);
                sp.setName(optionPrefix + MailImporterConfig.OPTION_MAIL_SERVICE_NAME + ".Default");
                sp.setDescription("Mail service name");
                sp.setStringValue("/mail/provider/" + providerName);
                componentConfiguration.addProperty(
                    UUIDConversion.toUID(UUIDs.newUUID()),
                    sp
                );
                // Default for mailbox
                sp = pm.newInstance(org.opencrx.kernel.base.jmi1.StringProperty.class);
                sp.setName(optionPrefix + MailImporterConfig.OPTION_MAIL_BOX + ".Default");
                sp.setDescription("Mailbox name");
                sp.setStringValue("INBOX");
                componentConfiguration.addProperty(
                    UUIDConversion.toUID(UUIDs.newUUID()),
                    sp
                );
                // Default for deleteImportedMessages
                org.opencrx.kernel.base.jmi1.BooleanProperty bp = pm.newInstance(org.opencrx.kernel.base.jmi1.BooleanProperty.class);
                bp.setName(optionPrefix + MailImporterConfig.OPTION_MAIL_DELETE_IMPORTED_MESSAGES + ".Default");
                bp.setDescription("Delete imported messages");
                bp.setBooleanValue(false);
                componentConfiguration.addProperty(
                	UUIDConversion.toUID(UUIDs.newUUID()),
                    bp
                );
                pm.currentTransaction().commit();
                componentConfiguration = adminSegment.getConfiguration(
                    COMPONENT_CONFIGURATION_ID
                );
            }
            // Get configuration
            for(int i = 0; i < 2; i++) {
                String suffix = new String[]{".Default", ""}[i];
                Collection<org.opencrx.kernel.base.jmi1.Property> properties = componentConfiguration.getProperty();
                for(org.opencrx.kernel.base.jmi1.Property property: properties) {
                    if((optionPrefix + MailImporterConfig.OPTION_MAIL_SERVICE_NAME + suffix).equals(property.getName())) {
                        mailServiceName = ((org.opencrx.kernel.base.jmi1.StringProperty)property).getStringValue();
                    }
                    else if((optionPrefix + MailImporterConfig.OPTION_MAIL_BOX + suffix).equals(property.getName())) {
                        mailbox = ((org.opencrx.kernel.base.jmi1.StringProperty)property).getStringValue();
                    }
                    else if((optionPrefix + MailImporterConfig.OPTION_MAIL_DELETE_IMPORTED_MESSAGES + suffix).equals(property.getName())) {
                        deleteImportedMessages = new Boolean(((org.opencrx.kernel.base.jmi1.BooleanProperty)property).isBooleanValue());
                    }
                }
            }
            if(mailServiceName == null) {
            	SysLog.warning("Could not get option " + MailImporterConfig.OPTION_MAIL_SERVICE_NAME + " from configuration. Either it does not exist or access level browse is not set to global. Fallback to system default. Used option prefix", optionPrefix);
            	SysLog.warning("Checked configuration properties", componentConfiguration.getProperty());
            }
            if(mailbox == null) {
            	SysLog.warning("Could not get option " + MailImporterConfig.OPTION_MAIL_BOX + " from configuration. Either it does not exist or access level browse is not set to global. Fallback to system default. Used option prefix", optionPrefix);
            	SysLog.warning("Checked configuration properties", componentConfiguration.getProperty());                
            }
        }
        catch(JmiServiceException e0) {
        	SysLog.info("Can not create default configuration", e0.getMessage());
            throw e0;
        }
        // Fallback to servlet config
        this.mailServiceName = mailServiceName == null ? 
        	"/mail/provider/" + providerName : 
        	mailServiceName;
        this.mailbox = mailbox == null ? 
        	"INBOX" : 
        	mailbox;
        this.deleteImportedMessages = deleteImportedMessages == null ? 
        	false : 
        	deleteImportedMessages.booleanValue();
    }
    
    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    public String toString(
    ) {
        StringBuffer tmp = new StringBuffer();
        tmp.append("EMailServerConfiguration{");
        tmp.append(OPTION_MAIL_SERVICE_NAME + "='").append(this.mailServiceName).append("', ");
        tmp.append(OPTION_MAIL_BOX + "='").append(this.mailbox).append("', ");
        tmp.append(OPTION_MAIL_DELETE_IMPORTED_MESSAGES + "='").append(this.deleteImportedMessages).append("', ");
        return tmp.toString();
    }

    /**
     * Get configured mail service name.
     * 
     * @return
     */
    public String getMailServiceName(
    ) {
        return this.mailServiceName;
    }
  
    /**
     * Get configured mailbox name.
     * 
     * @return
     */
    public String getMailbox(
    ) {
        return this.mailbox;
    }
  
    /**
     * If configured as true, delete imported messages.
     * 
     * @return
     */
    public boolean deleteImportedMessages(
    ) {
        return this.deleteImportedMessages;
    }
  
    //-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    private static final String COMPONENT_CONFIGURATION_ID = "MailImporterServlet";
        
    private static final String OPTION_MAIL_BOX = "mailbox";
    private static final String OPTION_MAIL_SERVICE_NAME = "mailServiceName";
    private static final String OPTION_MAIL_DELETE_IMPORTED_MESSAGES = "deleteImportedMessages";
    
    protected final String mailServiceName;
    protected final String mailbox;
    protected final boolean deleteImportedMessages;
  
}
