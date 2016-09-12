/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Description: Cloneable
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
package org.opencrx.kernel.backend;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.oasisopen.jmi1.RefContainer;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.marshalling.Marshaller;
import org.openmdx.base.persistence.cci.PersistenceHelper;

public class Cloneable extends AbstractImpl {

    //-------------------------------------------------------------------------
	public static void register(
	) {
		registerImpl(new Cloneable());
	}
	
    //-------------------------------------------------------------------------
	public static Cloneable getInstance(
	) throws ServiceException {
		return getInstance(Cloneable.class);
	}

	//-------------------------------------------------------------------------
	protected Cloneable(
	) {
		
	}
	
    //-------------------------------------------------------------------------
    protected Set<String> getReferenceFilter(
        String referenceFilterAsString
    ) {
        Set<String> referenceFilter = referenceFilterAsString == null ? null : new HashSet<String>();
        if(referenceFilter != null) {
            if(referenceFilterAsString != null) {
                StringTokenizer tokenizer = new StringTokenizer(referenceFilterAsString, " ;,", false);
                while(tokenizer.hasMoreTokens()) {
                    referenceFilter.add(
                        tokenizer.nextToken()
                    );
                }
            }
        }
        return referenceFilter;
    }
    
    //-------------------------------------------------------------------------
    public RefObject_1_0 cloneObject(
        RefObject_1_0 original,
        RefObject_1_0 target,
        String referenceName,
        Map<String,Marshaller> objectMarshallers,
        String referenceFilterAsString,
        org.opencrx.security.realm1.jmi1.User owningUser,
        List<org.opencrx.security.realm1.cci2.PrincipalGroup> owningGroup        
    ) throws ServiceException {
        Set<String> referenceFilter = this.getReferenceFilter(referenceFilterAsString);
        RefObject_1_0 cloned = this.cloneObject(
            original,
            target,
            referenceName,
            CLONE_EXCLUDE_ATTRIBUTES,
            objectMarshallers,
            referenceFilter,
            owningUser,
            owningGroup
        );
        return cloned;
    }
    
    //-------------------------------------------------------------------------
    private static class CloneContext {
    	
    	public CloneContext(
    		RefObject_1_0 target,
    		String targetReferenceName
    	) {
    		this.target = target;
    		this.targetReferenceName = targetReferenceName;
    	}
    	
    	public final RefObject_1_0 target;
    	public final String targetReferenceName;
    	
    }
    
    //-------------------------------------------------------------------------
    private static class CloneCallback implements Utils.TraverseObjectTreeCallback {

    	public CloneCallback(
            Set<String> excludeAttributes,
            Map<String,Marshaller> objectMarshallers,
            org.opencrx.security.realm1.jmi1.User owningUser,
            List<org.opencrx.security.realm1.cci2.PrincipalGroup> owningGroup
    	) {
    		this.excludeAttributes = excludeAttributes;
    		this.objectMarshallers = objectMarshallers;
    		this.owningUser = owningUser;
    		this.owningGroup = owningGroup;
    	}
    	
		@Override
		public Object visit(
			RefObject_1_0 object,
			Object context
		) throws ServiceException {
			CloneContext cloneContext = (CloneContext)context;
	        String objectType = object.refClass().refMofId();
	        // Clone
	        RefObject_1_0 clone = null;
	        if((this.objectMarshallers != null) && (this.objectMarshallers.get(objectType) != null)) {
	            clone = (RefObject_1_0)(this.objectMarshallers.get(objectType)).marshal(
	                object
	            );
	        }
	        else {
	            clone = PersistenceHelper.clone(object);
	        }
	        if(clone instanceof org.opencrx.kernel.base.jmi1.SecureObject) {
	        	if(this.owningUser != null) {
	        		((org.opencrx.kernel.base.jmi1.SecureObject)clone).setOwningUser(owningUser);
	        	}
	        	if(this.owningGroup != null) {
		            ((org.opencrx.kernel.base.jmi1.SecureObject)clone).getOwningGroup().clear();
		            ((org.opencrx.kernel.base.jmi1.SecureObject)clone).getOwningGroup().addAll(
		            	this.owningGroup
		            );
	        	}
	        }
	        RefContainer<?> container = (RefContainer<?>)cloneContext.target.refGetValue(
	        	cloneContext.targetReferenceName != null ? 
	        		cloneContext.targetReferenceName : 
	        			object.refGetPath().getParent().getBase()
	        );
	        container.refAdd(
	            org.oasisopen.cci2.QualifierType.REASSIGNABLE,
	            Utils.getUidAsString(),
	            clone
	        );
	        // Exclude attributes
	        if(this.excludeAttributes != null) {
	        	for(String excludeAttribute: this.excludeAttributes) {
	        		try {
	        			clone.refSetValue(excludeAttribute, null);
	        		}
	        		catch(Exception e) {}
	        	}
	        }
	        return new CloneContext(
	        	clone,
	        	null
	        );
		}

        private final Set<String> excludeAttributes;
        private final Map<String,Marshaller> objectMarshallers;
        private final org.opencrx.security.realm1.jmi1.User owningUser;
        private final List<org.opencrx.security.realm1.cci2.PrincipalGroup> owningGroup;
		
    }
    
    //-------------------------------------------------------------------------
    public RefObject_1_0 cloneObject(
        RefObject_1_0 object,
        RefObject_1_0 target,
        String referenceName,
        Set<String> excludeAttributes,
        Map<String,Marshaller> objectMarshallers,
        Set<String> referenceFilter,
        org.opencrx.security.realm1.jmi1.User owningUser,
        List<org.opencrx.security.realm1.cci2.PrincipalGroup> owningGroup
    ) throws ServiceException {
    	CloneContext cloneContext = (CloneContext)Utils.traverseObjectTree(
    		object, 
    		referenceFilter,
    		new CloneCallback(
    			excludeAttributes,
    			objectMarshallers,
    			owningUser,
    			owningGroup
    		),
    		new CloneContext(
    			target,
    			referenceName
    		)
    	);
        return cloneContext.target;
    }

    //-------------------------------------------------------------------------
    // Variables
    //-------------------------------------------------------------------------    
    public static final Set<String> CLONE_EXCLUDE_ATTRIBUTES =
        new HashSet<String>(Arrays.asList("activityNumber"));
    
    public static final Set<String> CLONE_EXCLUDE_COMPOSITE_REFERENCES =
        new HashSet<String>(Arrays.asList("view"));
    
    public static final int MANUAL_QUALIFIER_THRESHOLD = 10;
    
}

//--- End of File -----------------------------------------------------------
