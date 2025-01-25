/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: XmlImporter
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
package org.opencrx.kernel.backend;

import java.io.StringReader;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jmi.reflect.RefObject;
import jakarta.resource.cci.MappedRecord;

import org.oasisopen.cci2.QualifierType;
import org.oasisopen.jmi1.RefContainer;
import org.openmdx.application.dataprovider.cci.JmiHelper;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.Authority;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.naming.Path;
import org.openmdx.base.rest.cci.ObjectRecord;
import org.openmdx.base.rest.spi.Object_2Facade;
import org.xml.sax.InputSource;

/**
 * XmlImporter.
 *
 */
public class XmlImporter extends AbstractImpl {

	/**
	 * Register XmlImporter with backend.
	 * 
	 */
	public static void register(
	) {
		registerImpl(new XmlImporter());
	}
	
	/**
	 * Get registered XmlImporter.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static XmlImporter getInstance(
	) throws ServiceException {
		return getInstance(XmlImporter.class);
	}

	/**
	 * Constructor.
	 */
	protected XmlImporter(
	) {
		
	}
	
    /**
     * Import XML item.
     * 
     * @param item
     * @param locale
     * @param targetSegment
     * @param errors
     * @param report
     * @return
     * @throws ServiceException
     */
    /**
     * @param item
     * @param locale
     * @param targetSegment
     * @param errors
     * @param report
     * @return
     * @throws ServiceException
     */
    public RefObject importItem(
        byte[] item,
        short locale,
        org.openmdx.base.jmi1.Segment targetSegment,
        List<String> errors,
        List<String> report
    ) throws ServiceException {    	    	
    	PersistenceManager pm = JDOHelper.getPersistenceManager(targetSegment);
        Map<Path,ObjectRecord> data = new LinkedHashMap<Path,ObjectRecord>();
        String itemAsXml = null;
        try {
        	itemAsXml = new String(item, "UTF-8");
        } catch(Exception ignore) {}
        org.openmdx.application.xml.Importer.importObjects(
        	org.openmdx.application.xml.Importer.asTarget(data),
        	org.openmdx.application.xml.spi.ImportHelper.asSource(
                new InputSource(new StringReader(itemAsXml))
            )
        );
        // Load objects in multiple runs in order to resolve object dependencies.       
        Map<Path,RefObject> loadedObjects = new HashMap<Path,RefObject>();
        for(int runs = 0; runs < 5; runs++) {
            for(MappedRecord entry: data.values()) {
                // create new entries, update existing
                try {
                    RefObject_1_0 existing = null;
                    try {
                        existing = (RefObject_1_0)pm.getObjectById(
                            Object_2Facade.getPath(entry)
                        );
                    } catch(Exception ignore) {}
                    if(existing != null) {
                        loadedObjects.put(
                            existing.refGetPath(), 
                            existing
                        );                                    
                        JmiHelper.toRefObject(
                            entry,
                            existing,
                            loadedObjects, // object cache
                            null, // ignorable features
                            true // compareWithBeforeImage
                        );
                    } else {
                        String qualifiedClassName = Object_2Facade.getObjectClass(entry);
                        String packageName = qualifiedClassName.substring(0, qualifiedClassName.lastIndexOf(':'));
                        RefObject_1_0 newEntry = (RefObject_1_0)(pm.getObjectById(
                            Authority.class,
                            "xri://@openmdx*" + packageName.replace(":", ".")
                        )).refImmediatePackage().refClass(qualifiedClassName).refCreateInstance(null);
                        newEntry.refInitialize(false, false, false);
                        JmiHelper.toRefObject(
                            entry,
                            newEntry,
                            loadedObjects, // object cache
                            null,
                            true // compareWithBeforeImage
                        );
                        Path entryPath = Object_2Facade.getPath(entry);
                        Path parentIdentity = entryPath.getParent().getParent();
                        RefObject_1_0 parent = null;
                        try {
                            parent = loadedObjects.containsKey(parentIdentity) 
                            	? (RefObject_1_0)loadedObjects.get(parentIdentity) 
                            	: (RefObject_1_0)pm.getObjectById(parentIdentity);
                        } catch(Exception ignore) {}
                        if(parent != null) {
                            RefContainer<?> container = (RefContainer<?>)parent.refGetValue(
                            	entryPath.get(entryPath.size() - 2)
                            );
                            container.refAdd(
                                QualifierType.REASSIGNABLE,
                                entryPath.get(entryPath.size() - 1), 
                                newEntry
                            );
                        }
                        loadedObjects.put(
                        	entryPath, 
                            newEntry
                        );                                    
                    }
                } catch(Exception e) {
                    new ServiceException(e).log();
                    System.out.println("STATUS: " + e.getMessage() + " (for more info see log)");
                }
            }
        }
        return loadedObjects.isEmpty() 
        	? null 
        	: loadedObjects.values().iterator().next();
    }

    //-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    public static final String MIME_TYPE = "application/xml";
    public static final String FILE_EXTENSION = ".xml";
    
}

//--- End of File -----------------------------------------------------------
