/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TestDbUtils
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2012, CRIXP Corp., Switzerland
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
package test.org.opencrx.kernel.utils;

import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Properties;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.opencrx.kernel.tools.DbSearchReplace;
import org.opencrx.kernel.utils.DbSchemaUtils;
import org.openmdx.base.exception.ServiceException;

import test.org.opencrx.generic.AbstractTest;

@RunWith(Suite.class)
@SuiteClasses(
    {
        TestDbUtils.TestAll.class
    }
)

/**
 * TestDbSearchReplace
 */
public class TestDbUtils {

    //-----------------------------------------------------------------------
    @BeforeClass
    public static void initialize(
    ) throws ClassNotFoundException, SQLException {
    }
    
    //-----------------------------------------------------------------------
    public static class TestAll extends AbstractTest {
    	
		public TestAll(
		) {
			super(null);
		}
	
        @Test
        public void run(
        ) throws ServiceException, IOException, ClassNotFoundException, SQLException {
            this.testDbSearchReplace();
        }
		
	    /**
	     * @throws ServiceException
	     */
	    protected void testDbSearchReplace(
	    ) throws ServiceException, ClassNotFoundException, SQLException {
	        try {
	    		Class.forName(DbSchemaUtils.getJdbcDriverName(JDBC_URL));
	    		Properties props = new Properties();
	    		props.put("user", USERNAME);
	    		props.put("password", PASSWORD);
	    		Connection conn = DriverManager.getConnection(JDBC_URL, props);
	    		conn.setAutoCommit(true);    		        	
	        	DbSearchReplace.dbSearchReplace(
	        		conn, 
	        		"(.*)", // table name includes
	        		"(OOCKE1_ADDRESS.*)", // table name excludes
	        		"(.*)", // column name includes
	        		"(TEXT)|(DESCRIPTION)|(DETAILED_DESCRIPTION)|(MESSAGE_BODY)|(STRING_VALUE)|(BEFORE_IMAGE)", // column name excludes 
	        		"(/CRX/)",
	        		"/MYCOMPANY/", 
	        		true, // validateOnly 
	        		System.out
	        	);
	        	DbSearchReplace.dbSearchReplace(
	        		conn, 
	        		"(.*)", // table name includes
	        		"(OOCKE1_ADDRESS.*)", // table name excludes
	        		"(.*)", // column name includes
	        		"(TEXT)|(DESCRIPTION)|(DETAILED_DESCRIPTION)|(MESSAGE_BODY)|(STRING_VALUE)|(BEFORE_IMAGE)", // column name excludes 
	        		"(guest)",
	        		"newguest", 
	        		true, // validateOnly 
	        		System.out
	        	);
	        } finally {
	        }
	    }
	    
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
	protected final static String JDBC_URL = "jdbc:postgresql://localhost/CRX";
	protected final static String USERNAME = "postgres";
	protected final static String PASSWORD = "manager99";

}
