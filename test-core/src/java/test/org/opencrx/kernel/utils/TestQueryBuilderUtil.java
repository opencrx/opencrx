/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TestQueryBuilderUtil
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2013, CRIXP Corp., Switzerland
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
import java.text.ParseException;

import javax.naming.NamingException;
import javax.naming.spi.NamingManager;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.opencrx.kernel.utils.QueryBuilderUtil;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.kernel.lightweight.naming.NonManagedInitialContextFactoryBuilder;

import test.org.opencrx.generic.AbstractTest;

@RunWith(Suite.class)
@SuiteClasses(
    {
        TestQueryBuilderUtil.TestAll.class
    }
)

/**
 * TestQueryBuilderUtil
 */
public class TestQueryBuilderUtil {

    @BeforeClass
    public static void initialize(
    ) throws NamingException, ServiceException {
        if(!NamingManager.hasInitialContextFactoryBuilder()) {
            NonManagedInitialContextFactoryBuilder.install(null);
        }
    }
    
    public static class TestAll extends AbstractTest {
    	
		public TestAll(
		) {
			super(null);
		}

        @Test
        public void run(
        ) throws ServiceException, IOException, ParseException{
            this.testToClause();
        }
		
	    /**
	     * Test QueryBuilderUtil.Predicate.toClause()
	     * 
	     * @throws ServiceException
	     */
	    protected void testToClause(
	    ) throws ServiceException{
	        try {
	        	// Generic predicates
        		QueryBuilderUtil.Predicate isNotDisabledPredicate = new QueryBuilderUtil.OrPredicate(
        			Utils.getUidAsString(),
        			"This is a comment",
        			false // negate
        		).or(
	        		new QueryBuilderUtil.SingleValuedAttributePredicate(
	        			Utils.getUidAsString(),
	        			"This is a comment",
	        			"org:opencrx:kernel:generic:CrxObject:disabled",
	        			QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS,
	        			"NULL"
		        	)
        		).or(
	        		new QueryBuilderUtil.SingleValuedAttributePredicate(
	        			Utils.getUidAsString(),
	        			"This is a comment",
	        			"org:opencrx:kernel:generic:CrxObject:disabled",
	        			QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_IN,
	        			"('0')"
		        	)
        		);
        		// Test 1
	        	//   (dtype in ('org:opencrx:kernel:account1:LegalEntity')) and
	        	//   (ext_string0 is not null) and
	        	//   exists (select 0 from oocke1_account_ a_ where v.object_id = a_.object_id and a_.ext_code21 = 38) 
	        	{
		        	QueryBuilderUtil.Predicate predicate = new QueryBuilderUtil.AndPredicate(
		        		Utils.getUidAsString(),
		        		"This is a comment",
	        			false // negate		        		
		        	).and(
		        		new QueryBuilderUtil.InstanceOfPredicate(
		        			Utils.getUidAsString(),
		        			"This is a comment",
		        			"('org:opencrx:kernel:account1:LegalEntity')"
			        	)
		        	).and(
		        		new QueryBuilderUtil.SingleValuedAttributePredicate(
		        			Utils.getUidAsString(),
		        			"This is a comment",
		        			"org:opencrx:kernel:account1:Account:extString0",
		        			QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_NOT,
		        			"NULL"
			        	)
		        	).and(
		        		new QueryBuilderUtil.MultiValuedAttributePredicate(
		        			Utils.getUidAsString(),
		        			"This is a comment",
		        			"org:opencrx:kernel:account1:Account:extCode21",
		        			QueryBuilderUtil.MultiValuedAttributePredicate.Condition.IS_IN,
		        			"(38)"
			        	)
		        	);
		        	System.out.println("Clause 1:");
		        	System.out.println(
	        			predicate.toSql(
			        		"", 
			        		new Path("xri://@openmdx*org:opencrx.kernel.account1").getDescendant("provider", ":*", "segment", ":*", "account"), 
			        		"v"
			        	)
			        );
	                System.out.println(predicate.toXML());
	        	}
	        	// Test 2
	          	//   ((v.disabled is null) OR (v.disabled = '0'))
	          	//   and EXISTS (
	          	//   SELECT 0 FROM OOCKE1_ADDRESS addr WHERE addr.p$$parent = v.object_id AND
	          	//   ((addr.disabled is null) OR (addr.disabled = '0'))
	          	//   AND addr.dtype = 'org:opencrx:kernel:account1:PostalAddress'
	          	//   HAVING count(*) > 1)
	        	{
	        		QueryBuilderUtil.Predicate predicate = new QueryBuilderUtil.AndPredicate(
		        		Utils.getUidAsString(),
		        		"This is a comment",
	        			false // negate		        		
		        	).and(
		        		isNotDisabledPredicate
		        	).and(
		        		new QueryBuilderUtil.ReferencePredicate(
		        			Utils.getUidAsString(),
		        			"This is a comment",
		        			"org:opencrx:kernel:account1:Account:address", 
		        			QueryBuilderUtil.ReferencePredicate.Condition.EXISTS, 
		        			new QueryBuilderUtil.AndPredicate(
	    		        		Utils.getUidAsString(),
	    		        		"This is a comment",
	    	        			false // negate	    		        		
	    		        	).and(
	    		        		isNotDisabledPredicate
	    		        	).and(
    		        			new QueryBuilderUtil.InstanceOfPredicate(
				        			Utils.getUidAsString(),
				        			"This is a comment",
				        			"('org:opencrx:kernel:account1:PostalAddress')"
					        	)
	    		        	)
		        		).setHavingClause(
	    		        	"COUNT(*) > 1"
	    		        )
		        	);
		        	System.out.println("Clause 2:");
		        	System.out.println(
	        			predicate.toSql(
			        		"", 
			        		new Path("xri://@openmdx*org:opencrx.kernel.account1").getDescendant("provider", ":*", "segment", ":*", "account"), 
			        		"v"
			        	)
			        );
	                System.out.println(predicate.toXML());		        	
	        	}
	        	// Test 3
	        	//   not exists (
	        	//		select 0 from oocke1_account_ v_ where v.object_id = v_.object_id and
	        	//		(v_.external_link is not null)
	            //   ) or
	        	//   (v.vcard is null)
	        	{
	        		QueryBuilderUtil.Predicate predicate = new QueryBuilderUtil.OrPredicate(
			        	Utils.getUidAsString(),
			        	"This is a comment",
	        			false // negate			        	
			        ).or(
		        		new QueryBuilderUtil.MultiValuedAttributePredicate(
		        			Utils.getUidAsString(),
		        			"This is a comment",
		        			"org:opencrx:kernel:generic:CrxObject:externalLink",
		        			QueryBuilderUtil.MultiValuedAttributePredicate.Condition.IS_EMPTY,
		        			null
			        	)
			        ).or(
		        		new QueryBuilderUtil.SingleValuedAttributePredicate(
		        			Utils.getUidAsString(),
		        			"This is a comment",
		        			"org:opencrx:kernel:account1:Account:vcard",
		        			QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS,
		        			"NULL"
			        	)
			        );
		        	System.out.println("Clause 3:");
		        	System.out.println(
	        			predicate.toSql(
			        		"", 
			        		new Path("xri://@openmdx*org:opencrx.kernel.account1").getDescendant("provider", ":*", "segment", ":*", "account"), 
			        		"v"
			        	)
			        );
	                System.out.println(predicate.toXML());	        		
	        	}	        	
	        	// Test 4
	        	//	 exists (
	        	//	   select distinct(m.account) from oocke1_accountassignment m
	        	//		  where m.dtype = 'org:opencrx:kernel:account1:Member'
	        	//		  and ((m.disabled is null) OR (m.disabled = '0'))
	        	//		  and exists (
	        	//		    select 0 from oocke1_propertyset ps
	        	//		    where m.object_id = ps.p$$parent
	        	//		    and name = '<S|F>'
	        	//		  )
	        	//		  GROUP BY m.account HAVING count(*) > 1
	        	//		)
	        	{
	        		QueryBuilderUtil.Predicate predicate = new QueryBuilderUtil.CompositeParentPredicate(
			        	Utils.getUidAsString(),
			        	"This is a comment",
			        	QueryBuilderUtil.CompositeParentPredicate.Condition.EXISTS,
		        		new QueryBuilderUtil.ReferencePredicate(
		        			Utils.getUidAsString(),
		        			"This is a comment",
		        			"org:opencrx:kernel:account1:Account:accountMembership",
		        			QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
		        			new QueryBuilderUtil.ReferencePredicate(
		        				Utils.getUidAsString(),
		        				"This is a comment",
		        				"org:opencrx:kernel:account1:AccountMembership:member",
		        				QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
		        				new QueryBuilderUtil.AndPredicate(
		        			        Utils.getUidAsString(),
		        			        "This is a comment",
		                			false // negate		        			        
		        			    ).and(
	        			    		new QueryBuilderUtil.InstanceOfPredicate(
        			        			Utils.getUidAsString(),
        			        			"This is a comment",
        			        			"('org:opencrx:kernel:account1:Member')"
        				        	)
		        			    ).and(
		        			    	isNotDisabledPredicate
		        			    ).and(
	        			    		new QueryBuilderUtil.ReferencePredicate(
        		        				Utils.getUidAsString(),
        		        				"This is a comment",
        		        				"org:opencrx:kernel:generic:CrxObject:propertySet",
        		        				QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
        		        				new QueryBuilderUtil.AndPredicate(
        		        					Utils.getUidAsString(), 
        		        					"This is a comment", 
        		        					false
        		        				).and(
	        		        				new QueryBuilderUtil.SingleValuedAttributePredicate(
	        		        					Utils.getUidAsString(),
	        		        					"This is a comment",
	        		        					"org:opencrx:kernel:generic:PropertySet:name",
	        		        					QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_IN,
	        		        					"('<S|F>')"
	        		        				)
	        		        			).and(
        	        			    		new QueryBuilderUtil.ReferencePredicate(
    	        		        				Utils.getUidAsString(),
    	        		        				"This is a comment",
    	        		        				"org:opencrx:kernel:base:PropertySet:property",
    	        		        				QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
    	        		        				new QueryBuilderUtil.AndPredicate(
	            		        					Utils.getUidAsString(), 
	            		        					"This is a comment", 
	            		        					false
	            		        				).and(
	    	        		        				new QueryBuilderUtil.SingleValuedAttributePredicate(
			        		        					Utils.getUidAsString(),
			        		        					"This is a comment",
			        		        					"org:opencrx:kernel:base:Property:name",
			        		        					QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_IN,
			        		        					"('property name')"
			        		        				)
	    	        		        			).and(
	    	        		        				new QueryBuilderUtil.SingleValuedAttributePredicate(
			        		        					Utils.getUidAsString(),
			        		        					"This is a comment",
			        		        					"SUBSTRING(stringValue,1,4)",
			        		        					"org:opencrx:kernel:base:StringProperty:stringValue",
			        		        					QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_IN,
			        		        					"('property value')"
			        		        				)
	    	        		        			)
    	        		        			)
	        		        			)
        		        			)
		        			    )
		        			)
			        	).setHavingClause(
			        		"COUNT(*) > 1"
			        	)
			        );
		        	System.out.println("Clause 4:");
		        	System.out.println(
	        			predicate.toSql(
			        		"", 
			        		new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", ":*", "segment", ":*", "addressFilter", ":*", "filteredAddress", ":*"),
			        		"v"
			        	)
			        );
	                System.out.println(predicate.toXML());	        		
	        	}
	        	// Test 5
	        	//	 exists (
	        	//	   select distinct(m.account) from oocke1_accountassignment m
	        	//		  where m.dtype = 'org:opencrx:kernel:account1:Member'
	        	//		  and ((m.disabled is null) OR (m.disabled = '0'))
	        	//		  and exists (
	        	//		    select 0 from oocke1_propertyset ps
	        	//		    where m.object_id = ps.p$$parent
	        	//		    and name = '<S|F>'
	        	//		  )
	        	//		  GROUP BY m.account HAVING count(*) > 1
	        	//		)
	        	{
	        		QueryBuilderUtil.Predicate predicate = new QueryBuilderUtil.ReferencePredicate(
	        			Utils.getUidAsString(),
	        			"This is a comment",
	        			"org:opencrx:kernel:account1:Account:accountMembership",
	        			QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
	        			new QueryBuilderUtil.ReferencePredicate(
	        				Utils.getUidAsString(),
	        				"This is a comment",
	        				"org:opencrx:kernel:account1:AccountMembership:member",
	        				QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
	        				new QueryBuilderUtil.AndPredicate(
	        			        Utils.getUidAsString(),
	        			        "This is a comment",
	                			false // negate		        			        
	        			    ).and(
        			    		new QueryBuilderUtil.InstanceOfPredicate(
    			        			Utils.getUidAsString(),
    			        			"This is a comment",
    			        			"('org:opencrx:kernel:account1:Member')"
    				        	)
	        			    ).and(
	        			    	isNotDisabledPredicate
	        			    ).and(
        			    		new QueryBuilderUtil.ReferencePredicate(
    		        				Utils.getUidAsString(),
    		        				"This is a comment",
    		        				"org:opencrx:kernel:generic:CrxObject:propertySet",
    		        				QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
    		        				new QueryBuilderUtil.AndPredicate(
    		        					Utils.getUidAsString(), 
    		        					"This is a comment", 
    		        					false
    		        				).and(
        		        				new QueryBuilderUtil.SingleValuedAttributePredicate(
        		        					Utils.getUidAsString(),
        		        					"This is a comment",
        		        					"org:opencrx:kernel:generic:PropertySet:name",
        		        					QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_IN,
        		        					"('<S|F>')"
        		        				)
        		        			).and(
    	        			    		new QueryBuilderUtil.ReferencePredicate(
	        		        				Utils.getUidAsString(),
	        		        				"This is a comment",
	        		        				"org:opencrx:kernel:base:PropertySet:property",
	        		        				QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
	        		        				new QueryBuilderUtil.AndPredicate(
            		        					Utils.getUidAsString(), 
            		        					"This is a comment", 
            		        					false
            		        				).and(
    	        		        				new QueryBuilderUtil.SingleValuedAttributePredicate(
		        		        					Utils.getUidAsString(),
		        		        					"This is a comment",
		        		        					"org:opencrx:kernel:base:Property:name",
		        		        					QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_IN,
		        		        					"('property name')"
		        		        				)
    	        		        			).and(
    	        		        				new QueryBuilderUtil.SingleValuedAttributePredicate(
		        		        					Utils.getUidAsString(),
		        		        					"This is a comment",
		        		        					"SUBSTRING(stringValue,1,4)",
		        		        					"org:opencrx:kernel:base:StringProperty:stringValue",
		        		        					QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_IN,
		        		        					"('property value')"
		        		        				)
    	        		        			)
	        		        			)
        		        			)
    		        			)
	        			    )
	        			)
		        	).setHavingClause(
		        		"COUNT(*) > 1"
		        	);
		        	System.out.println("Clause 5:");
		        	System.out.println(
	        			predicate.toSql(
			        		"", 
			        		new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", ":*", "segment", ":*", "accountFilter", ":*", "filteredAccount", ":*"),
			        		"v"
			        	)
			        );
	                System.out.println(predicate.toXML());	        		
	        	}
	        	// Test 6
	        	{
					QueryBuilderUtil.Predicate predicate = new QueryBuilderUtil.ReferencePredicate(
	        			Utils.getUidAsString(),
	        			null, // comment
	        			"org:opencrx:kernel:generic:CrxObject:propertySet",
	        			QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
	        			new QueryBuilderUtil.AndPredicate(
	        				Utils.getUidAsString(), 
	        				null, // comment 
	        				false
	        			).and(
	        				new QueryBuilderUtil.SingleValuedAttributePredicate(
	        					Utils.getUidAsString(),
	        					null, // comment
	        					"org:opencrx:kernel:generic:PropertySet:name",
	        					QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_IN,
	        					"('Classification')"
	        				)
	        			).and(
    			    		new QueryBuilderUtil.ReferencePredicate(
  		        				Utils.getUidAsString(),
  		        				null, // comment
  		        				"org:opencrx:kernel:base:PropertySet:property",
  		        				QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
  		        				new QueryBuilderUtil.AndPredicate(
	        						Utils.getUidAsString(), 
	 		        				null, // comment 
	 		        				false
  		        				).and(
	        						new QueryBuilderUtil.OrPredicate(
   		        						Utils.getUidAsString(), 
   		 		        				null, // comment 
   		 		        				false
      		        				).or(
  		        						new QueryBuilderUtil.SingleValuedAttributePredicate(
	    		        					Utils.getUidAsString(),
	    		        					null, // comment
	    		        					"org:opencrx:kernel:base:Property:description",
	    		        					QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS,
	    		        					"NULL"
	    		        				) 		        						
      		        				).or(
  		        						new QueryBuilderUtil.SingleValuedAttributePredicate(
	    		        					Utils.getUidAsString(),
	    		        					null, // comment
	    		        					"org:opencrx:kernel:base:Property:description",
	    		        					QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_NOT_IN,
	    		        					"('D')"
	    		        				)	        						
      		        				)
  		        				).and(
	        						new QueryBuilderUtil.OrPredicate(
        		       			        Utils.getUidAsString(),
        		       			        null, // comment
        		               			false // negate							
        							).or(
    									new QueryBuilderUtil.SingleValuedAttributePredicate(
											Utils.getUidAsString(),
											null, // comment
											"org:opencrx:kernel:base:StringProperty:stringValue",
											QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_LIKE,
											"'%xyz%'"
										)								
        							)
  		        				)
  		        			)
    			    	)
	        		);
		        	System.out.println("Clause 6:");
		        	System.out.println(
	        			predicate.toSql(
			        		"", 
			        		new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", ":*", "segment", ":*", "accountFilter", ":*", "filteredAccount", ":*"), 
			        		"v"
			        	)
			        );
	                System.out.println(predicate.toXML());					
	        	}
	        	// Test 7
	        	//   (v.disabled is null or v.disabled = '0')
	        	//   AND v.object_id in (
	        	//     select a_.object_id from oocke1_account_ a_
	        	//     where a_.owner in (
	        	//       select SPLIT_PART (pg.object_id, '/', 4) || ':' || pg.name from oomse2_principal pg
	        	//       where pg.object_id in (
	        	//         select pg_.object_id from oomse2_principal_ pg_
	        	//         where pg_.is_member_of in (
	        	//           select p.object_id from oomse2_principal p
	        	//           where name = 'Users'
	        	//         )
	        	//       )
	        	//     )
	        	//   )
	        	{
	        		QueryBuilderUtil.Predicate predicate = new QueryBuilderUtil.OwnerPredicate(
			        	Utils.getUidAsString(),
			        	"This is a comment",
			        	QueryBuilderUtil.OwnerPredicate.Condition.EXISTS,
			        	new QueryBuilderUtil.ReferencePredicate(
			        		Utils.getUidAsString(),
			        		"This is a comment",
			        		"org:openmdx:security:realm1:Principal:isMemberOf",
			        		QueryBuilderUtil.ReferencePredicate.Condition.EXISTS, 
				        	new QueryBuilderUtil.SingleValuedAttributePredicate(
				        		Utils.getUidAsString(),
				        		"This is a comment",
				        		"org:openmdx:security:realm1:Principal:name",
				        		QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_IN,
				        		"('Unspecified')"
				        	)
			        	)
			        );
		        	System.out.println("Clause 7:");
		        	System.out.println(
	        			predicate.toSql(
			        		"", 
			        		new Path("xri://@openmdx*org:opencrx.kernel.account1").getDescendant("provider", ":*", "segment", ":*", "account"), 
			        		"v"
			        	)
			        );
	                System.out.println(predicate.toXML());	        		
	        	}
	        	// Test 8
	        	//   ((v.disabled is null) OR (v.disabled = '0'))
	        	//   and v.full_name IN (
	        	//     select distinct(a.full_name) from oocke1_account a
	        	//     where ((a.disabled is null) OR (a.disabled = '0'))
	        	//     GROUP BY a.full_name HAVING count(*) > 1
	        	//   ) 
	        	{
	        		QueryBuilderUtil.Predicate predicate = new QueryBuilderUtil.AndPredicate(
			        	Utils.getUidAsString(),
			        	"This is a comment",
	        			false // negate			        	
				    ).and(
				    	isNotDisabledPredicate
				    ).and(
				    	new QueryBuilderUtil.SelfJoinPredicate(
				    		Utils.getUidAsString(), 
				    		"This is a comment", 
				    		"org:opencrx:kernel:account1:Account:fullName",
				    		QueryBuilderUtil.SelfJoinPredicate.Condition.IS_IN,
				    		isNotDisabledPredicate
				    	).setHavingClause(
				    		"COUNT(*) > 1"
				    	)
				    );
		        	System.out.println("Clause 8:");
		        	System.out.println(
	        			predicate.toSql(
			        		"", 
			        		new Path("xri://@openmdx*org:opencrx.kernel.account1").getDescendant("provider", ":*", "segment", ":*", "account"), 
			        		"v"
			        	)
			        );
	                System.out.println(predicate.toXML());
	        	}
	        	// Test 9
	        	//   v.dtype='org:opencrx:kernel:account1:Contact' 
	        	//     and (v.disabled is null or v.disabled = false) 
	        	// 	   and v.object_id IN ( 
	        	//		 	select member.account from oocke1_accountassignment member 
	        	//		 	where ( 
	        	//		 			member.p$$parent = 'account/CRX/Standard/Company~Staff' or member.p$$parent IN ( 
	        	//		 				select member2.account from oocke1_accountassignment member2 
	        	//		 				where member2.p$$parent = 'account/CRX/Standard/Company~Staff' 
	        	//		 				and member2.dtype = 'org:opencrx:kernel:account1:Member' 
	        	//		 				and (member2.disabled is null or member2.disabled = false) 
	        	//		 		)	 
	        	//		 	) 
	        	//		 	and member.dtype = 'org:opencrx:kernel:account1:Member' 
	        	//		 	and (member.disabled is null or member.disabled = false) 
	        	//		 	and (member.member_role_0 = 11 or member.member_role_1 = 11 or member.member_role_2 = 11 or member.member_role_3 = 11 or member.member_role_4 = 11) 
	        	//		 )
	        	{
	        		QueryBuilderUtil.Predicate predicate = new QueryBuilderUtil.AndPredicate(
			        	Utils.getUidAsString(),
			        	"This is a comment",
	        			false // negate			        	
				    ).and(
				    	isNotDisabledPredicate
				    ).and(
				    	new QueryBuilderUtil.InstanceOfPredicate(
				    		Utils.getUidAsString(),
				    		"This is a comment",
				    		"('org:opencrx:kernel:account1:Contact')"
				    	)
				    ).and(
		        		new QueryBuilderUtil.ReferencePredicate(
		        			Utils.getUidAsString(),
		        			"This is a comment",
		        			"org:opencrx:kernel:account1:Account:accountMembership",
		        			QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
		        			new QueryBuilderUtil.AndPredicate(
	        					Utils.getUidAsString(),
		        				"This is a comment",
		            			false // negate		        				
	        				).and(
		        			    isNotDisabledPredicate		        			  
	        				).and(
		        				new QueryBuilderUtil.SingleValuedAttributePredicate(
		        					Utils.getUidAsString(), 
		        					"This is a comment", 
		        					"org:opencrx:kernel:account1:AccountMembership:distance", 
		        					QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_IN, 
		        					"(-1)"
			        			)
	        				).and(
			        			new QueryBuilderUtil.OrPredicate(
			        				Utils.getUidAsString(),
			        				"This is a comment",
			            			false // negate			        				
			        			).or(
				        			new QueryBuilderUtil.AndPredicate(
				        				Utils.getUidAsString(),
				        				"This is a comment",
				            			false // negate				        				
				        			).and(
			        			    	new QueryBuilderUtil.MultiValuedAttributePredicate(
			        			    		Utils.getUidAsString(), 
			        			    		"This is a comment", 
			        			    		"org:opencrx:kernel:account1:AccountMembership:memberRole",
			        			    		QueryBuilderUtil.MultiValuedAttributePredicate.Condition.IS_IN,
			        			    		"(11)"
			        			    	)		
				        			).and(
					        			new QueryBuilderUtil.ReferencePredicate(
					        				Utils.getUidAsString(),
					        				"This is a comment",
					        				"org:opencrx:kernel:account1:AccountMembership:accountFrom",
					        				QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
				        			    	new QueryBuilderUtil.IdentityPredicate(
					        			        Utils.getUidAsString(),
					        			        "This is a comment",
				        			    		QueryBuilderUtil.IdentityPredicate.Condition.IS_IN,
				        			    		"('account/CRX/Standard/Company~Staff')"
				        			    	)
				        			    )
				        			)
				        		).or(
				        			new QueryBuilderUtil.ReferencePredicate(
				        				Utils.getUidAsString(),
				        				"This is a comment",
				        				"org:opencrx:kernel:account1:AccountMembership:accountFrom",
				        				QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
			        			    	new QueryBuilderUtil.ReferencePredicate(
				        			        Utils.getUidAsString(),
				        			        "This is a comment",
				        			        "org:opencrx:kernel:account1:Account:accountMembership",
			        			    		QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
			        			    		new QueryBuilderUtil.AndPredicate(
				        			    		Utils.getUidAsString(),		        			    			
			        			    			"This is a comment",
			        		        			false // negate			        			    			
			        			    		).and(
			        			    			isNotDisabledPredicate
			        			    		).and(
		    			        				new QueryBuilderUtil.SingleValuedAttributePredicate(
	    				        					Utils.getUidAsString(), 
	    				        					"This is a comment", 
	    				        					"org:opencrx:kernel:account1:AccountMembership:distance", 
	    				        					QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_IN, 
	    				        					"(-1)"
	    				        				)
			        			    		).and(
		    				        			new QueryBuilderUtil.ReferencePredicate(
	    					        				Utils.getUidAsString(),
	    					        				"This is a comment",
	    					        				"org:opencrx:kernel:account1:AccountMembership:accountFrom",
	    					        				QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
	    				        			    	new QueryBuilderUtil.IdentityPredicate(
	    					        			        Utils.getUidAsString(),
	    					        			        "This is a comment",
	    				        			    		QueryBuilderUtil.IdentityPredicate.Condition.IS_IN,
	    				        			    		"('account/CRX/Standard/Company~Staff')"
	    				        			    	)
	    				        			    )
			        			    		)
			        			    	)
			        			    )
				        		)
			        		)
		        		)
				    );
		        	System.out.println("Clause 9:");
		        	System.out.println(
	        			predicate.toSql(
			        		"", 
			        		new Path("xri://@openmdx*org:opencrx.kernel.account1").getDescendant("provider", ":*", "segment", ":*", "account"), 
			        		"v"
			        	)
			        );
	                System.out.println(predicate.toXML());	        		
	        	}
	        	// Test 10
	        	// EXISTS (SELECT 0 FROM OOCKE1_ACCOUNT a WHERE v.sales_rep = a.object_id AND " + negate + " (UPPER(a.full_name) LIKE UPPER(" + s0 + ") OR UPPER(a.full_name) LIKE " + s1 + "))"
	        	{
	        		QueryBuilderUtil.Predicate predicate = new QueryBuilderUtil.ReferencePredicate(
        				Utils.getUidAsString(),
        				"This is a comment",
        				"org:opencrx:kernel:contract1:SalesContract:salesRep",
        				QueryBuilderUtil.ReferencePredicate.Condition.EXISTS,
        				new QueryBuilderUtil.OrPredicate(
        					Utils.getUidAsString(), 
        					"This is a comment",
        					true // negate
        				).or(
        					new QueryBuilderUtil.SingleValuedAttributePredicate(
        						Utils.getUidAsString(), 
        						"This is a comment", 
        						"UPPER(fullName)",
        						"org:opencrx:kernel:account1:Account:fullName", 
        						QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_LIKE, 
        						"UPPER(?s0)"
        					)
        				).or(
        					new QueryBuilderUtil.SingleValuedAttributePredicate(
        						Utils.getUidAsString(), 
        						"This is a comment",
        						"UPPER(fullName)",
        						"org:opencrx:kernel:account1:Account:fullName", 
        						QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_LIKE, 
        						"?s1"
        					)
        				)
    			    );
		        	System.out.println("Clause 10:");
		        	System.out.println(
	        			predicate.toSql(
			        		"", 
			        		new Path("xri://@openmdx*org:opencrx.kernel.contract1").getDescendant("provider", ":*", "segment", ":*", "salesOrder"), 
			        		"v"
			        	)
			        );
	                System.out.println(predicate.toXML());	        		
	        	}
	        } finally {
	        }
	    }
	    
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
		    
}
