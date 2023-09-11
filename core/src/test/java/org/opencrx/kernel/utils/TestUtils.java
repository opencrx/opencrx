/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TestQuery
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
package org.opencrx.kernel.utils;

import java.util.Collection;

import javax.naming.NamingException;
import javax.naming.spi.NamingManager;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.opencrx.generic.AbstractTest;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Contracts;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.contract1.jmi1.SalesOrder;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.lightweight.naming.LightweightInitialContextFactoryBuilder;

/**
 * TestUtils
 */
public class TestUtils extends AbstractTest {

    @BeforeEach
    public void initialize(
    ) throws NamingException, ServiceException {
        if(!NamingManager.hasInitialContextFactoryBuilder()) {
        	LightweightInitialContextFactoryBuilder.install(null);
        }
        entityManagerFactory = org.opencrx.kernel.utils.Utils.getPersistenceManagerFactory();
        pm = entityManagerFactory == null ? null : entityManagerFactory.getPersistenceManager();
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
     * Test Utils.traverseObjectTree
     * @throws ServiceException
     */
    @Test
    protected void testParseContentType(
    ) throws ServiceException {
    	String[] contentType1 = MimeUtils.parseContentType("application/octet-stream; name=CV_english_January.pdf");
    	Assertions.assertEquals("contentType[0]", "application/octet-stream", contentType1[0]);
    	Assertions.assertEquals("contentType[1]", "CV_english_January.pdf", contentType1[1]);
    	String[] contentType2 = MimeUtils.parseContentType("application/octet-stream; name=\"CV_english_January.pdf\"");
    	Assertions.assertEquals("contentType[0]", "application/octet-stream", contentType2[0]);
    	Assertions.assertEquals("contentType[1]", "CV_english_January.pdf", contentType2[1]);
    }
    
    /**
     * Test Utils.traverseObjectTree
     * @throws ServiceException
     */
    protected void testTraverseObjectTree(
    ) throws ServiceException{
        try {
        	org.opencrx.kernel.contract1.jmi1.Segment contractSegment = Contracts.getInstance().getContractSegment(pm, providerName, segmentName);
        	Collection<SalesOrder> salesOrders = contractSegment.getSalesOrder();
        	int count = 0;
        	for(SalesOrder salesOrder: salesOrders) {
	        	Utils.traverseObjectTree(
	        		salesOrder, 
	        		null, // referenceFilter 
	        		new Utils.TraverseObjectTreeCallback() {
						
						@Override
						public Object visit(
							RefObject_1_0 object, 
							Object context
						) throws ServiceException {
							System.out.println("Visit=" + object.refGetPath());
							return null;
						}
					},
	        		null
	        	);
	        	count++;
	        	if(count > 10) break;
        	} 
        } finally {
        }
    }

    /**
     * @throws ServiceException
     */
    @Test
    protected void testSecureObject(
    ) throws ServiceException {
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(pm, providerName, segmentName);
    	Collection<Account> accounts = accountSegment.getAccount();
    	int count = 0;
    	for(Account account: accounts) {
    		org.opencrx.security.realm1.jmi1.User user = account.getOwningUser();
    		String principalNameUser = user.refGetPath().getLastSegment().toString();
    		String principalName = principalNameUser.endsWith("." + SecurityKeys.USER_SUFFIX) ?
    			principalNameUser.substring(0, principalNameUser.indexOf(".")) :
    				principalNameUser;
    		UserHome userHome = UserHomes.getInstance().getUserHome(
    			principalName, 
    			accountSegment.refGetPath(), 
    			this.pm
    		);
    		Contact contact = userHome.getContact();
    		System.out.println("contact=" + contact);
    		count++;
    		if(count > 10) break;
    	}
    }
    
    /**
     * Test Utils.normalizeNewLines
     */
    @Test
    protected void testNormalizeNewLines(
    ) {
    	Assertions.assertTrue(
    		Utils.normalizeNewLines("Line #1\r\nLine #2\nLine #3\rLine #4").equals("Line #1\nLine #2\nLine #3\nLine #4"),
    		"newline normalize error"
    	);
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
	protected static String providerName = "CRX";
	protected static String segmentName = "Standard";
		    
}
