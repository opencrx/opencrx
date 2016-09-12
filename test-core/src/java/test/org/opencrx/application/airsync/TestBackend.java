/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TestBackend
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2010, CRIXP Corp., Switzerland
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
package test.org.opencrx.application.airsync;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;
import javax.naming.spi.NamingManager;
import javax.xml.transform.stream.StreamResult;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.opencrx.application.airsync.backend.impl.DatatypeMapper;
import org.opencrx.application.airsync.backend.impl.OpenCrxSyncBackend;
import org.opencrx.application.airsync.server.FolderSyncHandler;
import org.opencrx.application.airsync.server.GetItemEstimateHandler;
import org.opencrx.application.airsync.server.MoveItemsHandler;
import org.opencrx.application.airsync.server.PingHandler;
import org.opencrx.application.airsync.server.SyncHandler;
import org.opencrx.application.airsync.server.SyncRequest;
import org.opencrx.application.airsync.utils.DOMUtils;
import org.opencrx.application.airsync.utils.WbXMLTransformer;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.lightweight.naming.NonManagedInitialContextFactoryBuilder;
import org.openmdx.kernel.loading.Resources;
import org.w3c.dom.Document;

import test.org.opencrx.generic.AbstractTest;

@RunWith(Suite.class)
@SuiteClasses(
    {
        TestBackend.TestAll.class
    }
)

public class TestBackend {

    //-----------------------------------------------------------------------
	protected static class TestSyncRequest extends SyncRequest {

		public TestSyncRequest(
			String userId
		) {
			super();
			this.userId = userId;
		}

		@Override
		public File getTempDir(
		) throws ServiceException {
			try {
				return File.createTempFile(TestServerHandlers.class.getName(), ".tmp").getParentFile();
			} catch (IOException e) {
				throw new ServiceException(e);
			}
		}

		@Override
		public String getDeviceId(
		) {
			return "mypda";
		}

		
		@Override
		public String getUserId(
		) {
			return this.userId;
		}

		private final String userId;
		
	}
	
    //-----------------------------------------------------------------------
    @BeforeClass
    public static void initialize(
    ) throws NamingException, ServiceException {
        if(!NamingManager.hasInitialContextFactoryBuilder()) {
            NonManagedInitialContextFactoryBuilder.install(null);
        }
        entityManagerFactory = org.opencrx.kernel.utils.Utils.getPersistenceManagerFactory();
        syncBackend = new OpenCrxSyncBackend(
        	entityManagerFactory,
        	PROVIDER_NAME,
        	new DatatypeMapper()
        );
    }
    
    //-----------------------------------------------------------------------
    public static class TestAll extends AbstractTest {
    	
		public TestAll(
		) {
			super(TestBackend.entityManagerFactory);			
		}
	
        @Test
        public void run(
        ) throws ServiceException {
        	this.testFolderSyncHandler();
            this.testGetItemEstimateHandler();
//            this.testMoveItemsHandler();
            this.testPingHandler();
            this.testSyncHandler();
        }
		
        protected void testFolderSyncHandler(
        ) throws ServiceException {
        	try {        		
	        	FolderSyncHandler handler = new FolderSyncHandler(syncBackend, DEFAULT_PROFILE_PREFIX);
	        	SyncRequest request = new TestSyncRequest(USER_ID);
	    		URL url = Resources.getResource(
	    			"test/org/opencrx/application/airsync/FolderHierarchyRequest.xml"
	    		);
	    		InputStream is = url.openStream();
	    		Document docRequest = DOMUtils.parse(is);
	        	Document docResponse = handler.handle(request, docRequest);
                System.out.println();
                System.out.println("=-=-=-= testFolderSyncHandler =-=-=-=");
	        	WbXMLTransformer.transform(
	        		docResponse, 
	        		new StreamResult(System.out), 
	        		true
	        	);
                WbXMLTransformer.transformToWBXML(
                	docResponse, 
                	new ByteArrayOutputStream()
                );
                System.out.println();
                System.out.println("=-=-=-= testFolderSyncHandler =-=-=-=");
        	} catch(Exception e) {
        		throw new ServiceException(e);
        	}
        }
        
	    protected void testGetItemEstimateHandler(
	    ) throws ServiceException {
	    	try {
		    	GetItemEstimateHandler handler = new GetItemEstimateHandler(syncBackend, DEFAULT_PROFILE_PREFIX);
	        	SyncRequest request = new TestSyncRequest(USER_ID);
	    		URL url = Resources.getResource(
	    			"test/org/opencrx/application/airsync/GetItemEstimateRequest.xml"
	    		);
	    		InputStream is = url.openStream();
	    		Document docRequest = DOMUtils.parse(is);
	    		Document docResponse = handler.handle(request, docRequest);
                System.out.println();
                System.out.println("=-=-=-= testGetItemEstimateHandler =-=-=-=");
	        	WbXMLTransformer.transform(
	        		docResponse, 
	        		new StreamResult(System.out), 
	        		true
	        	);
                WbXMLTransformer.transformToWBXML(
                	docResponse, 
                	new ByteArrayOutputStream()
                );
                System.out.println();
                System.out.println("=-=-=-= testGetItemEstimateHandler =-=-=-=");	    		
        	} catch(Exception e) {
        		throw new ServiceException(e);
        	}
	    }

	    protected void testMoveItemsHandler(
	    ) throws ServiceException {
	    	try {
		    	MoveItemsHandler handler = new MoveItemsHandler(syncBackend, DEFAULT_PROFILE_PREFIX);
	        	SyncRequest request = new TestSyncRequest(USER_ID);
	    		URL url = Resources.getResource(
	    			"test/org/opencrx/application/airsync/MoveItemsRequest.xml"
	    		);
	    		InputStream is = url.openStream();
	    		Document docRequest = DOMUtils.parse(is);
	    		Document docResponse = handler.handle(request, docRequest);
                System.out.println();
                System.out.println("=-=-=-= testMoveItemsHandler =-=-=-=");
	        	WbXMLTransformer.transform(
	        		docResponse, 
	        		new StreamResult(System.out), 
	        		true
	        	);
                WbXMLTransformer.transformToWBXML(
                	docResponse, 
                	new ByteArrayOutputStream()
                );
                System.out.println();
                System.out.println("=-=-=-= testMoveItemsHandler =-=-=-=");	    		
        	} catch(Exception e) {
        		throw new ServiceException(e);
        	}
	    }

	    protected void testPingHandler(
	    ) throws ServiceException {
	    	try {
		    	PingHandler handler = new PingHandler(syncBackend, DEFAULT_PROFILE_PREFIX);
	        	SyncRequest request = new TestSyncRequest(USER_ID);
	        	// PingRequest1
	    		URL url = Resources.getResource(
	    			"test/org/opencrx/application/airsync/PingRequest1.xml"
	    		);
	    		InputStream is = url.openStream();
	    		Document docRequest = DOMUtils.parse(is);
	    		Document docResponse = handler.handle(request, docRequest);
                System.out.println();
                System.out.println("=-=-=-= testPingHandler (1) =-=-=-=");
	        	WbXMLTransformer.transform(
	        		docResponse, 
	        		new StreamResult(System.out), 
	        		true
	        	);
                WbXMLTransformer.transformToWBXML(
                	docResponse, 
                	new ByteArrayOutputStream()
                );
                System.out.println();
                System.out.println("=-=-=-= testPingHandler (1) =-=-=-=");	    		
	        	// PingRequest2
	    		url = Resources.getResource(
	    			"test/org/opencrx/application/airsync/PingRequest2.xml"
	    		);
	    		is = url.openStream();
	    		docRequest = DOMUtils.parse(is);
	    		docResponse = handler.handle(request, docRequest);
                System.out.println();
                System.out.println("=-=-=-= testPingHandler (2) =-=-=-=");
	        	WbXMLTransformer.transform(
	        		docResponse, 
	        		new StreamResult(System.out), 
	        		true
	        	);
                WbXMLTransformer.transformToWBXML(
                	docResponse, 
                	new ByteArrayOutputStream()
                );
                System.out.println();
                System.out.println("=-=-=-= testPingHandler (2) =-=-=-=");	    		
        	} catch(Exception e) {
        		throw new ServiceException(e);
        	}
	    }

	    protected void testSyncHandler(
	    ) throws ServiceException {
	    	try {
		    	SyncHandler handler = new SyncHandler(syncBackend, DEFAULT_PROFILE_PREFIX);
	        	SyncRequest request = new TestSyncRequest(USER_ID);
	        	// SyncRequest-GetAll.xml
	    		URL url = Resources.getResource(
	    			"test/org/opencrx/application/airsync/SyncRequest-GetAll.xml"
	    		);
	    		InputStream is = url.openStream();
	    		Document docRequest = DOMUtils.parse(is);
	    		Document docResponse = handler.handle(request, docRequest);
                System.out.println();
                System.out.println("=-=-=-= testSyncHandler (SyncRequest-GetAll.xml) =-=-=-=");
	        	WbXMLTransformer.transform(
	        		docResponse, 
	        		new StreamResult(System.out), 
	        		true
	        	);
                WbXMLTransformer.transformToWBXML(
                	docResponse, 
                	new ByteArrayOutputStream()
                );
                System.out.println();
                System.out.println("=-=-=-= testSyncHandler (SyncRequest-Add.xml) =-=-=-=");	    		
	        	// SyncRequest-GetAll.xml
	    		url = Resources.getResource(
	    			"test/org/opencrx/application/airsync/SyncRequest-Add.xml"
	    		);
	    		is = url.openStream();
	    		docRequest = DOMUtils.parse(is);
	    		docResponse = handler.handle(request, docRequest);
                System.out.println();
                System.out.println("=-=-=-= testSyncHandler (SyncRequest-Add.xml) =-=-=-=");
	        	WbXMLTransformer.transform(
	        		docResponse, 
	        		new StreamResult(System.out), 
	        		true
	        	);
                WbXMLTransformer.transformToWBXML(
                	docResponse, 
                	new ByteArrayOutputStream()
                );
                System.out.println();
                System.out.println("=-=-=-= testSyncHandler (SyncRequest-Add.xml) =-=-=-=");	    		
        	} catch(Exception e) {
        		throw new ServiceException(e);
        	}
	    }
	    
    }
    
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
	protected static final String PROVIDER_NAME = "CRX";
	protected static final String SEGMENT_NAME = "Standard";
	protected static final String USER_ID = SEGMENT_NAME + SyncBackend.DOMAIN_SEPARATOR + "guest";
	protected static final String DEFAULT_PROFILE_PREFIX = "AirSync~";

	protected static PersistenceManagerFactory entityManagerFactory = null;
	protected static SyncBackend syncBackend;
	
}
