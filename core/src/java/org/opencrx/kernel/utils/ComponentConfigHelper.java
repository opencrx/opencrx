/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ComponentConfigHelper
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2008, CRIXP Corp., Switzerland
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

import java.util.List;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.admin1.jmi1.ComponentConfiguration;
import org.openmdx.base.naming.Path;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.kernel.log.SysLog;

public abstract class ComponentConfigHelper {

    //-----------------------------------------------------------------------
    public static ComponentConfiguration getComponentConfiguration(
        String configurationId,
        String providerName,
        PersistenceManager pm,
        boolean autoCreate,
        String[][] initialProperties
    ) {
        ComponentConfiguration componentConfiguration = null;
        try {
            org.opencrx.kernel.admin1.jmi1.Segment adminSegment = 
                (org.opencrx.kernel.admin1.jmi1.Segment)pm.getObjectById(
                    new Path("xri://@openmdx*org.opencrx.kernel.admin1").getDescendant("provider", providerName, "segment", "Root")
                );
            try {
                componentConfiguration = adminSegment.getConfiguration(configurationId);
            } catch(Exception e) {}
            if(autoCreate && (componentConfiguration == null)) {
                componentConfiguration = pm.newInstance(ComponentConfiguration.class);
                componentConfiguration.setName(configurationId);
                pm.currentTransaction().begin();
                adminSegment.addConfiguration(
                    false,
                    configurationId,
                    componentConfiguration
                );
                if(initialProperties != null) {
	                for(String[] e: initialProperties) {
	                    org.opencrx.kernel.base.jmi1.StringProperty sp = pm.newInstance(org.opencrx.kernel.base.jmi1.StringProperty.class);
	                    sp.setName(e[0]);
	                    sp.setStringValue(e[1]);
	                    componentConfiguration.addProperty(
	                        UUIDConversion.toUID(UUIDs.newUUID()),
	                        sp
	                    );
	                }
                }
                pm.currentTransaction().commit();
                componentConfiguration = adminSegment.getConfiguration(
                    configurationId
                );
            }
        }
        catch(Exception e) {
        	SysLog.warning("Can not get component configuration", e);
        }
        return componentConfiguration;
    }
    
    //-----------------------------------------------------------------------
    public static org.opencrx.kernel.base.jmi1.StringProperty addComponentConfigProperty(
        String name,
        String stringValue,
        ComponentConfiguration configuration
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(configuration);
    	boolean isTxLocal = !pm.currentTransaction().isActive();
    	if(isTxLocal) {
    		pm.currentTransaction().begin();
    	}
        org.opencrx.kernel.base.jmi1.StringProperty sp = pm.newInstance(org.opencrx.kernel.base.jmi1.StringProperty.class);
        sp.setName(name);
        sp.setStringValue(stringValue);
        configuration.addProperty(
            UUIDConversion.toUID(UUIDs.newUUID()),
            sp
        );
        if(isTxLocal) {
        	pm.currentTransaction().commit();
        }
    	return sp;
    }
    
    //-----------------------------------------------------------------------
    public static org.opencrx.kernel.base.jmi1.StringProperty getComponentConfigProperty(
        String name,
        ComponentConfiguration configuration
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(configuration);
        org.opencrx.kernel.base.cci2.StringPropertyQuery query = (org.opencrx.kernel.base.cci2.StringPropertyQuery)pm.newQuery(org.opencrx.kernel.base.jmi1.StringProperty.class);
        query.name().equalTo(name);
    	List<org.opencrx.kernel.base.jmi1.StringProperty> properties = configuration.getProperty(query);
        return properties.isEmpty() ?
        	null :
        		properties.iterator().next();
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    
}
