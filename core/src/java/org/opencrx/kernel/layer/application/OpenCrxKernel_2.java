/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: OpenCrxKernel_2
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

package org.opencrx.kernel.layer.application;

import java.util.ArrayList;
import java.util.List;

import javax.resource.ResourceException;
import javax.resource.cci.Interaction;

import org.opencrx.kernel.generic.OpenCrxException;
import org.openmdx.base.collection.TreeSparseArray;
import org.openmdx.base.dataprovider.cci.DataproviderRequestProcessor;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.mof.cci.ModelElement_1_0;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.mof.spi.Model_1Factory;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.spi.PersistenceManagers;
import org.openmdx.base.resource.spi.ResourceExceptions;
import org.openmdx.base.resource.spi.RestInteractionSpec;
import org.openmdx.base.rest.cci.ObjectRecord;
import org.openmdx.base.rest.cci.QueryRecord;
import org.openmdx.base.rest.cci.RestConnection;
import org.openmdx.base.rest.cci.ResultRecord;
import org.openmdx.base.rest.spi.AbstractRestInteraction;
import org.openmdx.base.rest.spi.AbstractRestPort;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.log.SysLog;
import org.w3c.cci2.SparseArray;

/**
 * OpenCrxKernel_2
 *
 */
public class OpenCrxKernel_2 extends AbstractRestPort {

	/**
	 * Constructor.
	 * 
	 */
	public OpenCrxKernel_2(
	) {
		super();
	}
	
    /**
	 * @return the readOnlyTypes
	 */
	public SparseArray<String> getReadOnlyObjectType(
	) {
		return new TreeSparseArray<String>(this.readOnlyTypes);
	}

	/**
	 * @param readOnlyTypes the readOnlyTypes to set
	 */
	public void setReadOnlyObjectType(
		SparseArray<String> readOnlyObjectType
	) {
		this.readOnlyTypes = new ArrayList<String>(readOnlyObjectType.values());
	}

	/* (non-Javadoc)
	 * @see org.openmdx.base.resource.spi.Port#getInteraction(javax.resource.cci.Connection)
	 */
	@Override
    public Interaction getInteraction(
    	RestConnection connection
    ) throws ResourceException {
        return new RestInteraction(connection);
    }

    protected DataproviderRequestProcessor newDelegateRequestProcessor(
    	RestConnection connection
    ) throws ResourceException {
    	return new DataproviderRequestProcessor(
			 PersistenceManagers.toPrincipalChain(connection.getMetaData().getUserName()),
			 this.getDelegate()
		);
    }
    
    /**
     * RestInteraction
     *
     */
    public class RestInteraction extends AbstractRestInteraction {
      
        /**
         * Constructor.
         * 
         * @param connection
         * @throws ResourceException
         */
        public RestInteraction(
        	RestConnection connection
        ) throws ResourceException {
            super(connection, newDelegateInteraction(connection));
        }

	    protected DerivedReferences newDerivedReferences(
	    	RestInteractionSpec ispec
	    ) throws ResourceException {
	    	return new DerivedReferences(
	    		OpenCrxKernel_2.this.newDelegateRequestProcessor(this.getConnection()),
	    		OpenCrxKernel_2.this.readOnlyTypes
	    	);
	    }

	    public void testReferenceIsChangeable(
	        Path referencePath
	    ) throws ResourceException {
	    	try {
		    	Model_1_0 model = Model_1Factory.getModel();
		        // Reference must be changeable
		        ModelElement_1_0 reference = null;
		        try {
		            reference = model.getReferenceType(referencePath);
		        } catch(ServiceException e) {
		        	SysLog.warning("Reference not found in model", referencePath);
		        }          
		        if(
		            (reference != null) &&
		            !((Boolean)reference.isChangeable()).booleanValue()
		        ) {
	    			throw ResourceExceptions.initHolder(
	            		new ResourceException(
	            			"Reference is readonly. Can not add/remove objects.",
	        				BasicException.newEmbeddedExceptionStack(
	        	                OpenCrxException.DOMAIN,
	        	                OpenCrxException.REFERENCE_IS_READONLY,
	        	                new BasicException.Parameter("param0", referencePath)
			                 )
		                )
		            );	        	
		        }
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );	    		
	    	}
	    }

	    /* (non-Javadoc)
	     * @see org.openmdx.base.rest.spi.AbstractRestInteraction#get(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.cci.QueryRecord, org.openmdx.base.rest.cci.ResultRecord)
	     */
	    @Override
        public boolean get(
            RestInteractionSpec ispec, 
            QueryRecord request, 
            ResultRecord response
        ) throws ResourceException {
	        boolean isDerived = this.newDerivedReferences(ispec).getReply(
	        	ispec, 
	        	request,
	        	response
	        );
	        if(!isDerived) {
	            return super.get(
	                ispec,
	                request,
	                response
	            );
	        } else {
	        	return true;
	        }
	    }

	    /* (non-Javadoc)
	     * @see org.openmdx.base.rest.spi.AbstractRestInteraction#find(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.cci.QueryRecord, org.openmdx.base.rest.cci.ResultRecord)
	     */
	    @Override
        public boolean find(
            RestInteractionSpec ispec, 
            QueryRecord request, 
            ResultRecord response
        ) throws ResourceException {
	        boolean isDerived = this.newDerivedReferences(ispec).getReply(
	        	ispec, 
	        	request,
	        	response
	        );
	        if(!isDerived) {
	            return super.find(
	                ispec,
	                request,
	                response
	            );            
	        } else {
	        	return true;
	        }
	    }

	    /* (non-Javadoc)
	     * @see org.openmdx.base.rest.spi.AbstractRestInteraction#create(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.cci.ObjectRecord, org.openmdx.base.rest.cci.ResultRecord)
	     */
	    @Override
        public boolean create(
            RestInteractionSpec ispec, 
            ObjectRecord request, 
            ResultRecord response
        ) throws ResourceException {
	        this.testReferenceIsChangeable(
	            request.getResourceIdentifier().getParent()
	        );
	        return super.create(
	        	ispec,
	        	request,
	        	response
	        );
	    }
	
	    /* (non-Javadoc)
	     * @see org.openmdx.base.rest.spi.AbstractRestInteraction#delete(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.cci.ObjectRecord)
	     */
	    @Override
        public boolean delete(
            RestInteractionSpec ispec, 
            ObjectRecord request
        ) throws ResourceException {
	        this.testReferenceIsChangeable(
	            request.getResourceIdentifier().getParent()
	        );
	        return super.delete(
	            ispec,
	            request
	        );
	    }
	    
    }

    //-------------------------------------------------------------------------
    // Variables
    //-------------------------------------------------------------------------
    protected List<String> readOnlyTypes = null;

}
