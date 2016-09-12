/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TestWBXML
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.text.ParseException;

import javax.naming.NamingException;
import javax.naming.spi.NamingManager;
import javax.xml.transform.stream.StreamResult;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
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
        TestWBXML.TestAll.class
    }
)

public class TestWBXML {

    //-----------------------------------------------------------------------
    @BeforeClass
    public static void initialize(
    ) throws NamingException, ServiceException {
        if(!NamingManager.hasInitialContextFactoryBuilder()) {
            NonManagedInitialContextFactoryBuilder.install(null);
        }
    }
    
    //-----------------------------------------------------------------------
    public static class TestAll extends AbstractTest {
    	
		public TestAll(
		) {
			super(null);
		}
	
        @Test
        public void run(
        ) throws ServiceException, IOException, ParseException {
        	this.testReadWBXML();
            this.testReadWriteWBXML();
        }
		
        protected void testReadWBXML(
        ) {
        	for(String wbxmlResource: WBXML_RESOURCES) {
        		Document doc = null;
                System.out.println();
                System.out.println("=-=-=-= " + wbxmlResource + " =-=-=-=");
        		// Transform from WBXML
        		try {
	        		URL url = Resources.getResource(wbxmlResource);
	        		InputStream is = url.openStream();
        			doc = WbXMLTransformer.transformFromWBXML(is);
        		} catch(Exception e) {
        			new ServiceException(e).log();
        			fail("Error transforming WBXML to document " + wbxmlResource);	        			
        		}
        		// Print document as XML
        		try {
        			WbXMLTransformer.transform(
        				doc, 
        				new StreamResult(System.out),
        				true
        			);
        		} catch(Exception e) {
        			new ServiceException(e).log();
        			fail("Error printing document " + wbxmlResource);	        				        			
        		}
                System.out.println();
                System.out.println("=-=-=-= " + wbxmlResource + " =-=-=-=");
                System.out.println();
        	}
        }
        
	    protected void testReadWriteWBXML(
	    ) throws ServiceException{
        	for(String xmlResource: XML_RESOURCES) {
                System.out.println();
                System.out.println("=-=-=-= " + xmlResource + " =-=-=-=");
        		// Parse XML
        		Document doc = null;
        		try {
	        		URL url = Resources.getResource(xmlResource);
	        		InputStream is = url.openStream();
	        		doc = DOMUtils.parse(is);
        		} catch(Exception e) {
        			new ServiceException(e).log();
        			fail("Error parsing file " + xmlResource);
        		}
        		// Transform from document to WBXML
        		ByteArrayOutputStream out = new ByteArrayOutputStream();	        		
        		try {
	        		WbXMLTransformer.transformToWBXML(doc, out);
	        		out.close();		        		
        		} catch(Exception e) {
        			new ServiceException(e).log();
        			fail("Error transforming document to WBXML " + xmlResource);
        		}
        		// Transform from WBXML to document
        		ByteArrayInputStream in = new ByteArrayInputStream(out.toByteArray());
        		try {
        			doc = WbXMLTransformer.transformFromWBXML(in);
        		} catch(Exception e) {
        			new ServiceException(e).log();
        			fail("Error transforming WBXML to document " + xmlResource);	        			
        		}
        		// Print document
        		try {
        			WbXMLTransformer.transform(
        				doc, 
        				new StreamResult(System.out),
        				true
        			);
        		} catch(Exception e) {
        			new ServiceException(e).log();
        			fail("Error printing document " + xmlResource);	        				        			
        		}
                System.out.println();
                System.out.println("=-=-=-= " + xmlResource + " =-=-=-=");
                System.out.println();
        	}
	    }

    }
    
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
	protected static String[] WBXML_RESOURCES = {
		"test/org/opencrx/application/airsync/contact_sync_wm61.wbxml",
		"test/org/opencrx/application/airsync/ex2k7provResp1.wbxml",
		"test/org/opencrx/application/airsync/ex2k7provResp2.wbxml",
		"test/org/opencrx/application/airsync/exchange_foldersync.wbxml",
		"test/org/opencrx/application/airsync/iphone_sync1.wbxml",
		"test/org/opencrx/application/airsync/iphone_sync2.wbxml",
		"test/org/opencrx/application/airsync/iphone_sync3.wbxml",
		"test/org/opencrx/application/airsync/iphoneProvReq1.wbxml",
		"test/org/opencrx/application/airsync/iphoneProvReq2.wbxml",
		"test/org/opencrx/application/airsync/provisionRequest.wbxml",
		"test/org/opencrx/application/airsync/provisionResponse.wbxml",
		"test/org/opencrx/application/airsync/provisionResponse2.wbxml",
		"test/org/opencrx/application/airsync/settings.wbxml",
		"test/org/opencrx/application/airsync/settingsRequest.wbxml",
		"test/org/opencrx/application/airsync/settingsResponse.wbxml",
		"test/org/opencrx/application/airsync/sync_request_wm61.wbxml",
		"test/org/opencrx/application/airsync/tom_prov_resp.wbxml",
		"test/org/opencrx/application/airsync/SyncRequest-1.wbxml"
	};
	
	protected static String[] XML_RESOURCES = {
		"test/org/opencrx/application/airsync/brokenSyncRequest.xml",
		"test/org/opencrx/application/airsync/CalSyncAdd.xml",
		"test/org/opencrx/application/airsync/CalSyncRequest.xml",
		"test/org/opencrx/application/airsync/CalSyncRequest2.xml",
		"test/org/opencrx/application/airsync/contactSyncRequest.xml",
		"test/org/opencrx/application/airsync/contactSyncRequest2.xml",
        "test/org/opencrx/application/airsync/contactSyncRequest3.xml",
        "test/org/opencrx/application/airsync/contactSyncResponse3.xml",
		"test/org/opencrx/application/airsync/EmailSyncRequest.xml",
		"test/org/opencrx/application/airsync/EmailSyncRequest2.xml",
		"test/org/opencrx/application/airsync/EmailSyncRequest3.xml",
        "test/org/opencrx/application/airsync/EventSyncRequest.xml",
		"test/org/opencrx/application/airsync/ExchangeMailSyncResponse.xml",
		"test/org/opencrx/application/airsync/FolderHierarchyRequest.xml",
        "test/org/opencrx/application/airsync/FolderSyncResponse.xml",
		"test/org/opencrx/application/airsync/GetItemEstimateRequest.xml",
        "test/org/opencrx/application/airsync/GetItemEstimateRequest2.xml",
        "test/org/opencrx/application/airsync/GetItemEstimateResponse2.xml",
        "test/org/opencrx/application/airsync/ItemOperationsResponse.xml",
        "test/org/opencrx/application/airsync/MoveItemsRequest.xml",
		"test/org/opencrx/application/airsync/ProvisionRequest1.xml",
		"test/org/opencrx/application/airsync/ProvisionRequestProtocol2.5.xml",
		"test/org/opencrx/application/airsync/SearchRequest.xml",
		"test/org/opencrx/application/airsync/SettingsGet.xml",
		"test/org/opencrx/application/airsync/SettingsRequest.xml",
		"test/org/opencrx/application/airsync/SyncRequest.xml",		
		"test/org/opencrx/application/airsync/SyncRequest-GetAll.xml",		
		"test/org/opencrx/application/airsync/SyncResponse.xml",
		"test/org/opencrx/application/airsync/SyncResponse-GetAll.xml",
	};
	
}
