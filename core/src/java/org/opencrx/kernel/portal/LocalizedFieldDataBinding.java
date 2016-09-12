/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: LocalizedFieldDataBinding
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2014, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.portal;

import java.util.List;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jmi.reflect.RefObject;

import org.opencrx.kernel.generic.cci2.LocalizedFieldQuery;
import org.opencrx.kernel.generic.jmi1.CrxObject;
import org.opencrx.kernel.generic.jmi1.LocalizedField;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.DataBinding;

/**
 * LocalizedFieldDataBinding
 *
 */
public class LocalizedFieldDataBinding extends DataBinding {

    /**
     * Constructor.
     * 
     */
    public LocalizedFieldDataBinding(
    	String parameterString    		
    ) {
		short usage = 0;
    	if(parameterString != null) {
    		String[] parameters = parameterString.split(";");
    		for(String parameter: parameters) {
    			if(parameter.startsWith("usage=")) {
    				try {
    					usage = Short.valueOf(parameter.substring(6));
    				} catch(Exception ignore) {}
    			}
    		}
    	}
   		this.usage = usage;
    }

    /**
     * Get localized field name for given qualified feature name. Qualified feature
     * names are of the form <ui qualified feature name>!<localized field name>.
     *
     * @param qualifiedFeatureName
     * @return
     */
    protected String getLocalizedFieldName(
    	String qualifiedFeatureName
    ) {
    	return qualifiedFeatureName.substring(qualifiedFeatureName.indexOf("!") + 1);    	
    }

    /**
     * Find localized field with given name and current user locale.
     * 
     * @param object
     * @param name
     * @param app
     * @return
     */
    protected LocalizedField findLocalizedField(
    	RefObject object,
    	String name,
    	ApplicationContext app
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(object);
    	if(object instanceof CrxObject) {
    		CrxObject crxObject = (CrxObject)object;
    		LocalizedFieldQuery query = (LocalizedFieldQuery)pm.newQuery(LocalizedField.class);
    		query.name().equalTo(name);
    		query.locale().equalTo(app.getCurrentLocaleAsIndex());
    		query.usage().equalTo(this.usage);
    		List<LocalizedField> localizedFields = crxObject.getLocalizedField(query);
    		return localizedFields.isEmpty()
    			? null
    			: localizedFields.iterator().next();
    	} else {
    		return null;
    	}
    }

    /* (non-Javadoc)
     * @see org.openmdx.portal.servlet.DataBinding#getValue(javax.jmi.reflect.RefObject, java.lang.String, org.openmdx.portal.servlet.ApplicationContext)
     */
    @Override
    public Object getValue(
        RefObject object, 
        String qualifiedFeatureName,
        ApplicationContext app
    ) {
    	String name = this.getLocalizedFieldName(qualifiedFeatureName);
        LocalizedField localizedField = this.findLocalizedField(object, name, app);
        return localizedField == null
        	? null
        	: localizedField.getLocalizedValue();
    }

    /* (non-Javadoc)
     * @see org.openmdx.portal.servlet.DataBinding#setValue(javax.jmi.reflect.RefObject, java.lang.String, java.lang.Object, org.openmdx.portal.servlet.ApplicationContext)
     */
    @Override
    public void setValue(
        RefObject object, 
        String qualifiedFeatureName, 
        Object newValue,
        ApplicationContext app
    ) {
    	String name = this.getLocalizedFieldName(qualifiedFeatureName);
        LocalizedField localizedField = this.findLocalizedField(object, name, app);
        if(localizedField == null) {
        	PersistenceManager pm = JDOHelper.getPersistenceManager(object);
        	if(object instanceof CrxObject) {
        		CrxObject crxObject = (CrxObject)object;
        		localizedField = pm.newInstance(LocalizedField.class);
        		crxObject.addLocalizedField(
        			Utils.getUidAsString(),
        			localizedField
        		);
        	}
        }
        if(localizedField != null) {
    		localizedField.setName(name);
    		localizedField.setLocale(app.getCurrentLocaleAsIndex());
    		localizedField.setLocalizedValue((String)newValue);
    		localizedField.setUsage(this.usage);
        }
    }

    private final short usage;
    
}
