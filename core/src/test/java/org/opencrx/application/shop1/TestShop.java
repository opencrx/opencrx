/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TestShop
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
package org.opencrx.application.shop1;

import java.io.IOException;
import java.text.ParseException;
import java.util.Collections;

import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.opencrx.application.shop1.service.ShopServiceImpl;
import org.opencrx.application.shop1.test.TestShopService;
import org.opencrx.generic.AbstractTest;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.lightweight.naming.LightweightInitialContextFactoryBuilder;

/**
 * TestShop
 */
public class TestShop extends AbstractTest {

    @BeforeEach
    public void initialize(
    ) throws NamingException, ServiceException {
    	if(true) {
    		// In-process deployment with LightweightContainer
        	LightweightInitialContextFactoryBuilder.install(
  				Collections.singletonMap(
						"org.openmdx.comp.env.jdbc_opencrx_CRX",
						"jdbc:postgresql:\\/\\/localhost:5432\\/opencrx-test?user=opencrx-test&password=secret&driverClassName=org.postgresql.Driver&maxPoolSize=10"
					)
              );
	        entityManagerFactory = Utils.getPersistenceManagerFactory();
	        pm = entityManagerFactory == null ? null : entityManagerFactory.getPersistenceManager("admin-Standard", null);
    	}
    }

    @AfterEach
    public void tearDown(
    ){
    	if(this.pm != null) {
    		this.pm.close();
    	}
        this.pm = null;
    }
    
    /**
     * Get shop service tester.
     * 
     * @return
     */
    protected TestShopService getShopServiceTester(
    ) {
    	// Initialize Configuration
    	try{
    		Class<?> clazz = Class.forName("org.opencrx.kernel.aop2.Configuration");
    		clazz.getDeclaredConstructor().newInstance();
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
        return shopServiceTester;
    }
    
    /**
     * @throws ServiceException
     * @throws IOException
     * @throws ParseException
     */
    @Test
    public void testProducts(
    ) throws ServiceException, IOException, ParseException{
        TestShopService shopServiceTester = this.getShopServiceTester();
        org.opencrx.application.shop1.cci2.ReturnStatusT returnStatus = null;
		returnStatus = shopServiceTester.testProducts();
		Assertions.assertEquals(0, returnStatus.getReturnCode(), "testProducts");
    }

    /**
     * @throws ServiceException
     * @throws IOException
     * @throws ParseException
     */
    @Test
    public void testCustomers(
    ) throws ServiceException, IOException, ParseException{
        TestShopService shopServiceTester = this.getShopServiceTester();
        org.opencrx.application.shop1.cci2.ReturnStatusT returnStatus = null;
		returnStatus = shopServiceTester.testCustomers();
		Assertions.assertTrue(returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.DUPLICATE || returnStatus.getReturnCode() == BasicException.Code.NONE, "testCustomers");
    }

    /**
     * @throws ServiceException
     * @throws IOException
     * @throws ParseException
     */
    @Test
    public void testDocuments(
    ) throws ServiceException, IOException, ParseException{
        TestShopService shopServiceTester = this.getShopServiceTester();
        org.opencrx.application.shop1.cci2.ReturnStatusT returnStatus = null;
		returnStatus = shopServiceTester.testDocuments();
		Assertions.assertEquals(0, returnStatus.getReturnCode(), "testDocuments");
    }

    /**
     * @throws ServiceException
     * @throws IOException
     * @throws ParseException
     */
    @Test
    public void testLegalEntities(
    ) throws ServiceException, IOException, ParseException{
        TestShopService shopServiceTester = this.getShopServiceTester();
        org.opencrx.application.shop1.cci2.ReturnStatusT returnStatus = null;
		returnStatus = shopServiceTester.testLegalEntities();
		Assertions.assertTrue(returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.NONE, "testSalesOrders");
    }

    /**
     * @throws ServiceException
     * @throws IOException
     * @throws ParseException
     */
    @Test
    public void testSalesOrders(
    ) throws ServiceException, IOException, ParseException{
        TestShopService shopServiceTester = this.getShopServiceTester();
        org.opencrx.application.shop1.cci2.ReturnStatusT returnStatus = null;
		returnStatus = shopServiceTester.testSalesOrders();
		Assertions.assertTrue(returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.NONE, "testSalesOrders");
    }

    /**
     * @throws ServiceException
     * @throws IOException
     * @throws ParseException
     */
    @Test
    public void testInvoices(
    ) throws ServiceException, IOException, ParseException{
        TestShopService shopServiceTester = this.getShopServiceTester();
        org.opencrx.application.shop1.cci2.ReturnStatusT returnStatus = null;
		returnStatus = shopServiceTester.testInvoices();
		Assertions.assertTrue(returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.NONE, "testInvoices");
    }

    /**
     * @throws ServiceException
     * @throws IOException
     * @throws ParseException
     */
    @Test
    public void testVouchers(
    ) throws ServiceException, IOException, ParseException{
        TestShopService shopServiceTester = this.getShopServiceTester();
        org.opencrx.application.shop1.cci2.ReturnStatusT returnStatus = null;
		returnStatus = shopServiceTester.testVouchers();
		Assertions.assertTrue(returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.NONE, "testVouchers");
    }

    /**
     * @throws ServiceException
     * @throws IOException
     * @throws ParseException
     */
    @Test
    public void testCodeValues(
    ) throws ServiceException, IOException, ParseException{
        TestShopService shopServiceTester = this.getShopServiceTester();
        org.opencrx.application.shop1.cci2.ReturnStatusT returnStatus = null;
		returnStatus = shopServiceTester.testCodeValues();
		Assertions.assertTrue(returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.NONE, "testCodeValues");
    }

    /**
     * @throws ServiceException
     * @throws IOException
     * @throws ParseException
     */
    @Test
    public void testActivities(
    ) throws ServiceException, IOException, ParseException{
        TestShopService shopServiceTester = this.getShopServiceTester();
        org.opencrx.application.shop1.cci2.ReturnStatusT returnStatus = null;
		returnStatus = shopServiceTester.testActivities();
		Assertions.assertTrue(returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.NONE, "testActivities");
    }

    /**
     * @throws ServiceException
     * @throws IOException
     * @throws ParseException
     */
    @Test
    public void testRegisterCustomer(
    ) throws ServiceException, IOException, ParseException{
        TestShopService shopServiceTester = this.getShopServiceTester();
        org.opencrx.application.shop1.cci2.ReturnStatusT returnStatus = null;
		returnStatus = shopServiceTester.testRegisterCustomer();            
		Assertions.assertTrue(returnStatus.getReturnCode() == BasicException.Code.NOT_FOUND || returnStatus.getReturnCode() == BasicException.Code.DUPLICATE || returnStatus.getReturnCode() == BasicException.Code.NONE, "testRegisterCustomer");
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    protected static PersistenceManagerFactory entityManagerFactory = null;
	protected static String providerName = "CRX";
	protected static String segmentName = "Standard";
	
}
