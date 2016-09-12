/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TestShop
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
package test.org.opencrx.application.shop1;

import java.io.IOException;
import java.text.ParseException;

import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;
import javax.naming.spi.NamingManager;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.opencrx.application.shop1.service.ShopServiceImpl;
import org.opencrx.application.shop1.test.TestShopService;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.lightweight.naming.NonManagedInitialContextFactoryBuilder;

import test.org.opencrx.generic.AbstractTest;

@RunWith(Suite.class)
@SuiteClasses(
    {
        TestShop.TestAll.class
    }
)

/**
 * TestShop
 */
public class TestShop {

    @BeforeClass
    public static void initialize(
    ) throws NamingException, ServiceException {
        if(!NamingManager.hasInitialContextFactoryBuilder()) {
            NonManagedInitialContextFactoryBuilder.install(null);
        }    	
       	entityManagerFactory = org.opencrx.kernel.utils.Utils.getPersistenceManagerFactory();
    }
    
    /**
     * TestAll
     *
     */
    public static class TestAll extends AbstractTest {
    	
		public TestAll(
		) {
			super(TestShop.entityManagerFactory);
		}
	
        /**
         * @throws ServiceException
         * @throws IOException
         * @throws ParseException
         */
        @Test
        public void run(
        ) throws ServiceException, IOException, ParseException{
        	// Initialize Configuration
        	try{
        		Class<?> clazz = Class.forName("org.opencrx.kernel.aop2.Configuration");
        		clazz.newInstance();
        	} catch(Exception ignore) {}
            org.opencrx.application.shop1.cci2.ShopService shopService =
		        new ShopServiceImpl(
		    		pm,
		    		providerName,
		    		segmentName,
		    		"TestShop",
		    		false,
		    		false,
		    		new org.opencrx.application.shop1.datatypes.DatatypeMappers()
		        );
            TestShopService shopServiceTester = new TestShopService(shopService);
            org.opencrx.application.shop1.cci2.ReturnStatusT returnStatus = null;
    		returnStatus = shopServiceTester.testProducts();
    		assertEquals("testProducts", 0, returnStatus.getReturnCode());
    		returnStatus = shopServiceTester.testCustomers();
    		assertTrue("testCustomers", returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.DUPLICATE || returnStatus.getReturnCode() == BasicException.Code.NONE);
    		returnStatus = shopServiceTester.testDocuments();
    		assertEquals("testDocuments", 0, returnStatus.getReturnCode());
    		returnStatus = shopServiceTester.testLegalEntities();
    		assertEquals("testLegalEntities", 0, returnStatus.getReturnCode());
    		returnStatus = shopServiceTester.testSalesOrders();
    		assertTrue("testSalesOrders", returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.NONE);
    		returnStatus = shopServiceTester.testInvoices();
    		assertTrue("testInvoices", returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.NONE);
    		returnStatus = shopServiceTester.testVouchers();
    		assertTrue("testVouchers", returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.NONE);
    		returnStatus = shopServiceTester.testCodeValues();
    		assertTrue("testCodeValues", returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.NONE);
    		returnStatus = shopServiceTester.testActivities();
    		assertTrue("testActivities", returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.NONE);
    		returnStatus = shopServiceTester.testRegisterCustomer();            
    		assertTrue("testRegisterCustomer", returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.DUPLICATE || returnStatus.getReturnCode() == BasicException.Code.NONE);
        }
		
    }
    
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    protected static PersistenceManagerFactory entityManagerFactory = null;
	protected static String providerName = "CRX";
	protected static String segmentName = "Standard";
		    
}
