/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: PropertiesHelper
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2016, CRIXP Corp., Switzerland
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

import java.util.Date;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.base.jmi1.BooleanProperty;
import org.opencrx.kernel.base.jmi1.DateTimeProperty;
import org.opencrx.kernel.base.jmi1.IntegerProperty;
import org.opencrx.kernel.base.jmi1.ReferenceProperty;
import org.opencrx.kernel.base.jmi1.StringProperty;
import org.opencrx.kernel.generic.jmi1.PropertySet;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;

/**
 * PropertiesHelper
 *
 */
public abstract class PropertiesHelper {
    
	/**
	 * Add reference property to propertySet.
	 * 
	 * @param propertySet
	 * @param value
	 * @throws ServiceException
	 */
	public static void addProperty(
		PropertySet propertySet,
		String name,
		BasicObject value
	) throws ServiceException {
		if(value != null) {
			PersistenceManager pm = JDOHelper.getPersistenceManager(propertySet);
			ReferenceProperty property = pm.newInstance(ReferenceProperty.class);
			property.setName(name);
			property.setReferenceValue(value);
			propertySet.addProperty(
				Utils.getUidAsString(),
				property
			);
		}
	}

	/**
	 * Add boolean property to propertySet.
	 * 
	 * @param propertySet
	 * @param value
	 * @throws ServiceException
	 */
	public static void addProperty(
		PropertySet propertySet,
		String name,
		Boolean value
	) throws ServiceException {
		if(value != null) {
			PersistenceManager pm = JDOHelper.getPersistenceManager(propertySet);
			BooleanProperty property = pm.newInstance(BooleanProperty.class);
			property.setName(name);
			property.setBooleanValue(value);
			propertySet.addProperty(
				Utils.getUidAsString(),
				property
			);
		}
	}
	
	/**
	 * Add string property to propertySet.
	 * 
	 * @param propertySet
	 * @param value
	 * @throws ServiceException
	 */
	public static void addProperty(
		PropertySet propertySet,
		String name,
		String value
	) throws ServiceException {
		if(value != null) {
			PersistenceManager pm = JDOHelper.getPersistenceManager(propertySet);
			StringProperty property = pm.newInstance(StringProperty.class);
			property.setName(name);
			property.setStringValue(value);
			propertySet.addProperty(
				Utils.getUidAsString(),
				property
			);
		}
	}

	/**
	 * Add string property to propertySet.
	 * 
	 * @param propertySet
	 * @param value
	 * @throws ServiceException
	 */
	public static void addProperty(
		PropertySet propertySet,
		String name,
		Date value
	) throws ServiceException {
		if(value != null) {
			PersistenceManager pm = JDOHelper.getPersistenceManager(propertySet);
			DateTimeProperty property = pm.newInstance(DateTimeProperty.class);
			property.setName(name);
			property.setDateTimeValue(value);
			propertySet.addProperty(
				Utils.getUidAsString(),
				property
			);
		}
	}

	/**
	 * Add integer property to propertySet.
	 * 
	 * @param propertySet
	 * @param value
	 * @throws ServiceException
	 */
	public static void addProperty(
		PropertySet propertySet,
		String name,
		Number value
	) throws ServiceException {
		if(value != null) {
			PersistenceManager pm = JDOHelper.getPersistenceManager(propertySet);
			IntegerProperty property = pm.newInstance(IntegerProperty.class);
			property.setName(name);
			property.setIntegerValue(value.intValue());
			propertySet.addProperty(
				Utils.getUidAsString(),
				property
			);
		}
	}
	
}
