/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: QueryBuilderUtil
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
package org.opencrx.kernel.utils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import javax.jdo.Constants;

import org.openmdx.application.configuration.Configuration;
import org.openmdx.application.dataprovider.layer.persistence.jdbc.Database_1;
import org.openmdx.application.spi.PropertiesConfigurationProvider;
import org.openmdx.base.dataprovider.layer.persistence.jdbc.dbobject.DbObject;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.mof.cci.ModelElement_1_0;
import org.openmdx.base.mof.cci.ModelHelper;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.mof.spi.Model_1Factory;
import org.openmdx.base.naming.Path;
import org.openmdx.base.text.conversion.JavaBeans;
import org.openmdx.kernel.exception.BasicException;

/**
 * QueryBuilderUtil. Utility which allows to generate and parse 
 * schema compliant query clauses. The clauses are typically used
 * for low-level database access, e.g. for the sql clause filter
 * properties in AccountFilters, ContractFilters, ProductFilters, 
 * or ActivityFilters.
 *
 */
public abstract class QueryBuilderUtil {

	/**
	 * Get database plug-ins configuration.
	 * 
	 * @return
	 */
	protected static Database_1[] getDatabasePlugIns(
	) throws ServiceException {
		if(databasePlugIns == null) {
			// Prepare database plugins for namespace Kernel and Security
			Database_1 kernelDatabasePlugIn = new Database_1();
			{
		    	Properties sourceConfiguration = new Properties();
		    	sourceConfiguration.put(Constants.PROPERTY_NAME, "Kernel");
		    	Configuration configuration = PropertiesConfigurationProvider.getConfiguration(
		    		sourceConfiguration, 
		    		"PERSISTENCE"
		    	);
		    	kernelDatabasePlugIn.activate((short) 0, configuration, null);
			}
			Database_1 securityDatabasePlugIn = new Database_1();
			{
		    	Properties sourceConfiguration = new Properties();
		    	sourceConfiguration.put(Constants.PROPERTY_NAME, "Security");
		    	Configuration configuration = PropertiesConfigurationProvider.getConfiguration(
		    		sourceConfiguration, 
		    		"PERSISTENCE"
		    	);
		    	securityDatabasePlugIn.activate((short) 0, configuration, null);
			}
			databasePlugIns = new Database_1[]{kernelDatabasePlugIn, securityDatabasePlugIn};			                                 
		}
		return databasePlugIns;
	}

	/**
	 * Predicate.
	 *
	 */
	public static abstract class Predicate {
		
		/**
		 * Constructor.
		 * 
		 */
		public Predicate(
		) {
			super();
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 */
		public Predicate(
			String id,
			String description
		) {
			this.id = id;
			this.description = description;
		}
		
		/**
		 * @return the id
		 */
		public String getId() {
			return id;
		}
		/**
		 * @param id the id to set
		 */
		public void setId(String id) {
			this.id = id;
		}
		/**
		 * @return the description
		 */
		public String getDescription() {
			return description;
		}
		/**
		 * @param description the description to set
		 */
		public void setDescription(String description) {
			this.description = description;
		}
		
		/**
		 * Find predicate with given id. If the predicate has nested
		 * predicates, they are searched recursively.
		 * 
		 * @param id
		 * @return
		 */
		public Predicate findPredicate(
			String id
		) {
			return id.equals(this.id) ? this : null;		
		}

		/**
		 * Format predicate as SQL clause.
		 * 
		 * @param indent
		 * @param accessPath
		 * @param tableAlias
		 * @param databasePlugIns
		 * @return
		 * @throws ServiceException
		 */
		public abstract String toSql(
			String indent,
			Path accessPath,
			String tableAlias
		) throws ServiceException;

		/**
		 * Render predicate as XML.
		 * 
		 * @return
		 * @throws ServiceException
		 */
		public String toXML(
		) throws ServiceException {
			return JavaBeans.toXML(this);			
		}
		
		/**
		 * Parse predicate from XML.
		 * 
		 * @param predicate
		 * @return
		 * @throws ServiceException
		 */
		public static Predicate fromXML(
			String predicate
		) throws ServiceException {
			return (Predicate)JavaBeans.fromXML(predicate);
		}

		public static final String TAB = "    "; 
		protected String id;
		protected String description;
	}

	/**
	 * ComplexPredicate.
	 *
	 */
	public static abstract class ComplexPredicate extends Predicate {
		
		/**
		 * Constructor.
		 * 
		 */
		public ComplexPredicate(
		) {
			super();
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public ComplexPredicate(
			String id,
			String description,
			boolean negate
		) {
	        super(
	        	id,
	        	description
	        );
	        this.negate = negate;
        }

		/**
		 * Add given predicate.
		 * 
		 * @param predicate
		 */
		protected ComplexPredicate addPredicate(
			Predicate predicate
		) {
			if(this.predicates == null) {
				 this.predicates = new ArrayList<Predicate>();				
			}
			this.predicates.add(predicate);
			return this;
		}

		/**
		 * Remove predicate with given id.
		 * 
		 * @param id
		 * @return
		 */
		public Predicate removePredicate(
			String id
		) {
			if(this.predicates != null) {
				for(Iterator<Predicate> i = this.predicates.iterator(); i.hasNext(); ) {
					Predicate predicate = i.next();
					if(id.equals(predicate.getId())) {
						i.remove();
						return predicate;
					}
				}
			}
			return null;
		}

		/* (non-Javadoc)
		 * @see org.opencrx.kernel.utils.QueryBuilderUtil.Predicate#findPredicate(java.lang.String)
		 */
		@Override
		public Predicate findPredicate(
			String id
		) {
			Predicate f = super.findPredicate(id);
			if(f == null) {
				for(Predicate predicate: this.predicates) {
					f = predicate.findPredicate(id);
					if(f != null) {
						return f;
					}
				}
				return null;
			} else {
				return f;
			}
		}

		/**
		 * @return the predicates
		 */
		public List<Predicate> getPredicates() {
			return predicates;
		}
		
		/**
		 * @param predicates the predicates to set
		 */
		public void setPredicates(List<Predicate> predicates) {
			this.predicates = predicates;
		}
		
		/**
		 * @return the negate
		 */
		public boolean isNegate() {
			return negate;
		}

		/**
		 * @param negate the negate to set
		 */
		public void setNegate(boolean negate) {
			this.negate = negate;
		}

		protected boolean negate;
		protected List<Predicate> predicates;

	}

	/**
	 * AndPredicate.
	 *
	 */
	public static class AndPredicate extends ComplexPredicate {

		/**
		 * Constructor.
		 * 
		 */
		public AndPredicate(
		) {
			super();
		}
		
		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public AndPredicate(
			String id,
			String description,
			boolean negate
		) {
	        super(
	        	id,
	        	description,
	        	negate
	        );
        }

		/**
		 * Add predicate to AND clause.
		 *  
		 * @param predicate
		 * @return
		 */
		public AndPredicate and(
			Predicate predicate
		) {
			return (AndPredicate)super.addPredicate(predicate);
		}

		/* (non-Javadoc)
		 * @see org.opencrx.kernel.portal.wizard.QueryBuilderController.Predicate#toClause(java.lang.String, org.openmdx.base.naming.Path, java.lang.String)
		 */
		@Override
        public String toSql(
        	String indent,
        	Path accessPath,
        	String tableAlias
        ) throws ServiceException {
			String clause = "";
			if(this.getDescription() != null) {
				clause += indent + "/* " + this.getDescription() + " */\n";
			}
			if(this.getPredicates() != null && !this.getPredicates().isEmpty()) {
				clause += indent + (this.negate ? "NOT " : "") + "(\n";
				int count = 0;
		        for(Predicate predicate: this.getPredicates()) {
		        	clause += (count == 0 ? "" : " AND\n") + predicate.toSql(indent + TAB, accessPath, tableAlias);
		        	count++;
		        }
		        clause += "\n";
		        clause += indent + ")";
			}
	        return clause;
        }

	}

	/**
	 * OrPredicate.
	 *
	 */
	public static class OrPredicate extends ComplexPredicate {

		/**
		 * Constructor.
		 * 
		 */
		public OrPredicate(
		) {
			super();
		}

		/**
		 * Add predicate to OR clause.
		 *  
		 * @param predicate
		 * @return
		 */
		public OrPredicate or(
			Predicate predicate
		) {
			return (OrPredicate)super.addPredicate(predicate);
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public OrPredicate(
			String id,
			String description,
			boolean negate
		) {
	        super(
	        	id,
	        	description,
	        	negate
	        );
        }

		/* (non-Javadoc)
		 * @see org.opencrx.kernel.portal.wizard.QueryBuilderController.Predicate#toClause(java.lang.String, org.openmdx.base.naming.Path, java.lang.String)
		 */
		@Override
        public String toSql(
        	String indent,
        	Path accessPath,
        	String tableAlias
        ) throws ServiceException {
			String clause = "";
			if(this.getDescription() != null) {
				clause += indent + "/* " + this.getDescription() + " */\n";
			}
			if(this.getPredicates() != null && !this.getPredicates().isEmpty()) {
				clause += indent + (this.negate ? "NOT " : "") + "(\n";
				int count = 0;
		        for(Predicate predicate: this.getPredicates()) {
		        	clause += (count == 0 ? "" : " OR\n") + predicate.toSql(indent + TAB, accessPath, tableAlias);
		        	count++;
		        }
		        clause += "\n";
		        clause += indent + ")";
			}
	        return clause;
		}

	}

	/**
	 * InstanceOfPredicate
	 *
	 */
	public static class InstanceOfPredicate extends Predicate {

		/**
		 * Constructor.
		 * 
		 */
		public InstanceOfPredicate(
		) {
			super();
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public InstanceOfPredicate(
			String id,
			String description,
			String values
		) {
	        super(
	        	id,
	        	description
	        );
	        this.values = values;
        }

		/**
		 * @return the values
		 */
		public String getValues() {
			return values;
		}

		/**
		 * @param values the values to set
		 */
		public void setValues(String values) {
			this.values = values;
		}

		@Override
        public String toSql(
        	String indent, 
        	Path accessPath, 
        	String tableAlias
        ) throws ServiceException {
			String clause = "";
			if(this.getDescription() != null) {
				clause += indent + "/* " + this.getDescription() + " */\n";
			}			
			clause += indent + "(" + tableAlias + ".dtype IN " + this.values + ")";
			return clause;
        }

		protected String values;

	}

	/**
	 * IdentityPredicate
	 *
	 */
	public static class IdentityPredicate extends Predicate {

		/**
		 * Condition
		 *
		 */
		public enum Condition {
			IS_IN,
			PARENT_IS_IN
		};

		/**
		 * Constructor.
		 * 
		 */
		public IdentityPredicate(
		) {
			super();
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param description
		 * @param condition
		 * @param value
		 */
		public IdentityPredicate(
			String id,
			String description,
			Condition condition,
			String value
		) {
	        super(
	        	id,
	        	description
	        );
	        this.condition = condition;
	        this.values = value;
        }

		/**
		 * Get name for parent column.
		 * 
		 * @param databasePlugIns
		 * @return
		 * @throws ServiceException
		 */
		protected String getColumnNameParent(
		) throws ServiceException {
			return getDatabasePlugIns()[0].getDelegate().getColumnName(
				null, // conn
				"parent", 
				0, 
				false, // indexSuffixIfZero 
				false, // ignoreReservedWords 
				true // markAsPrivate
			);
		}

		/**
		 * @return the values
		 */
		public String getValues() {
			return values;
		}

		/**
		 * @param values the values to set
		 */
		public void setValues(String values) {
			this.values = values;
		}

		/**
		 * @return the condition
		 */
		public Condition getCondition() {
			return condition;
		}

		/**
		 * @param condition the condition to set
		 */
		public void setCondition(Condition condition) {
			this.condition = condition;
		}

		@Override
        public String toSql(
        	String indent, 
        	Path accessPath, 
        	String tableAlias
        ) throws ServiceException {
			String clause = "";
			if(this.getDescription() != null) {
				clause += indent + "/* " + this.getDescription() + " */\n";
			}
			switch(this.condition) {
				case IS_IN:
					clause += indent + "(" + tableAlias + ".object_id IN " + this.values + ")";
					break;
				case PARENT_IS_IN:
					clause += indent + "(" + tableAlias + "." + this.getColumnNameParent() + " IN " + this.values + ")";
					break;
			}
			return clause;
        }

		protected String values;
		protected Condition condition;

	}

	/**
	 * FeaturePredicate.
	 *
	 */
	public static abstract class FeaturePredicate extends Predicate {

		/**
		 * Constructor.
		 * 
		 */
		public FeaturePredicate(
		) {
			super();
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public FeaturePredicate(
			String id,
			String description,
			String qualifiedFeatureName
		) {
			super(
				id,
				description
			);
	        this.qualifiedFeatureName = qualifiedFeatureName;
        }

		/**
		 * Get feature.
		 * 
		 * @return
		 * @throws ServiceException
		 */
		protected ModelElement_1_0 getFeature(
		) throws ServiceException {
			Model_1_0 model = Model_1Factory.getModel();
			return model.getElement(this.getQualifiedFeatureName());
		}
		
		/**
		 * Get column name for feature.
		 * 
		 * @return
		 * @throws ServiceException
		 */
		protected String getColumnName(
		) throws ServiceException {
			String columnName = getDatabasePlugIns()[0].getDelegate().getColumnName(
				null, // conn 
				(String)this.getFeature().getName(), 
				0, // index 
				false, // indexSuffixIfZero 
				true, // ignoreReservedWords 
				false // markAsPrivate
			);
			return columnName;
		}

		/**
		 * Get upper bound for embedded feature. Return null if feature is not embedded.
		 * 
		 * @param databasePlugIns
		 * @return
		 * @throws ServiceException
		 */
		protected Integer getEmbeddedFeature(
		) throws ServiceException {
			return getDatabasePlugIns()[0].getDelegate().getEmbeddedFeature(
				(String)this.getFeature().getName()
			);
		}

		/**
		 * Get column name for feature.
		 * 
		 * @param index
		 * @param databasePlugIns
		 * @return
		 * @throws ServiceException
		 */
		protected String getColumnName(
			int index
		) throws ServiceException {
			String columnName = getDatabasePlugIns()[0].getDelegate().getColumnName(
				null, // conn 
				(String)this.getFeature().getName(), 
				index, // index 
				true, // indexSuffixIfZero 
				true, // ignoreReservedWords 
				false // markAsPrivate
			);
			return columnName;
		}

		/**
		 * Get access path for referenced type.
		 * 
		 * @param reference
		 * @param accessPath
		 * @return
		 * @throws ServiceException
		 */
		protected Path getReferencedTypeAccessPath(
			ModelElement_1_0 reference,
			Path accessPath
		) throws ServiceException {
			Model_1_0 model = reference.getModel();
			Path referencedTypeAccessPath = model.getIdentityPattern(
				model.getElement(reference.getType())
			);
			if(referencedTypeAccessPath == null) {
				try {
					referencedTypeAccessPath = model.getIdentityPattern(
						model.getElement(model.getReferenceType(accessPath).getType())
					);
				} catch(Exception ignore) {}
				if(referencedTypeAccessPath == null) {
					referencedTypeAccessPath = accessPath.size() % 2 == 0
						? accessPath.getChild(":*")
						: accessPath;
				}
				referencedTypeAccessPath = referencedTypeAccessPath.getDescendant((String)reference.getName());
			}
			return referencedTypeAccessPath;			
		}
		
		/**
		 * Get dbObject for given access path.
		 * 
		 * @param accessPath
		 * @return
		 * @throws ServiceException
		 */
		protected DbObject getDbObject(
			Path accessPath
		) throws ServiceException {
			for(Database_1 databasePlugIn: getDatabasePlugIns()) {
				try {
					DbObject dbObject = databasePlugIn.getDelegate().getDbObject(
						null, // conn 
						accessPath, 
						null, // filter 
						true // isQuery
					);
					if(
						dbObject.getConfiguration().getDbObjectForUpdate1() != null || 
						dbObject.getConfiguration().getDbObjectForQuery1() != null
					) {
						return dbObject;
					}
				} catch(Exception ignore) {}
			}
			return null;
		}

		/**
		 * @return the qualifiedFeatureName
		 */
		public String getQualifiedFeatureName() {
			return qualifiedFeatureName;
		}

		/**
		 * @param qualifiedFeatureName the qualifiedFeatureName to set
		 */
		public void setQualifiedFeatureName(String qualifiedFeatureName) {
			this.qualifiedFeatureName = qualifiedFeatureName;
		}

		protected String qualifiedFeatureName;
		
	}

	/**
	 * IsTruePredicate
	 *
	 */
	public static class IsTruePredicate extends Predicate {

		/**
		 * Constructor.
		 * 
		 */
		public IsTruePredicate(
		) {
			super();
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param description
		 */
		public IsTruePredicate(
			String id,
			String description
		) {
			super(
				id, 
				description
			);
		}
		
		/* (non-Javadoc)
		 * @see org.opencrx.kernel.utils.QueryBuilderUtil.Predicate#toSql(java.lang.String, org.openmdx.base.naming.Path, java.lang.String, org.openmdx.application.dataprovider.layer.persistence.jdbc.Database_1[])
		 */
		@Override
        public String toSql(
        	String indent, 
        	Path accessPath, 
        	String tableAlias
        ) throws ServiceException {
			String clause = indent + "(1=1)";
			if(this.getDescription() != null) {
				clause += " /* " + this.getDescription() + " */";
			}
			return clause;
        }

	}
	
	/**
	 * IsFalsePredicate
	 *
	 */
	public static class IsFalsePredicate extends Predicate {

		/**
		 * Constructor.
		 * 
		 */
		public IsFalsePredicate(
		) {
			super();
		}
		
		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param description
		 */
		public IsFalsePredicate(
			String id,
			String description
		) {
			super(
				id, 
				description
			);			
		}

		/* (non-Javadoc)
		 * @see org.opencrx.kernel.utils.QueryBuilderUtil.Predicate#toSql(java.lang.String, org.openmdx.base.naming.Path, java.lang.String, org.openmdx.application.dataprovider.layer.persistence.jdbc.Database_1[])
		 */
		@Override
        public String toSql(
        	String indent, 
        	Path accessPath, 
        	String tableAlias
        ) throws ServiceException {
			String clause = "";
			clause = "(1=0)";
			if(this.getDescription() != null) {
				clause += " /* " + this.getDescription() + " */";
			}
			return clause;
        }
	}

	/**
	 * SingleValuedAttributePredicate.
	 *
	 */
	public static class SingleValuedAttributePredicate extends FeaturePredicate {

		/**
		 * Condition
		 *
		 */
		public enum Condition {
			IS_LIKE,
			IS_UNLIKE,
			IS_LESS_OR_EQUAL,
			IS_LESS,
			IS_NOT_IN,
			IS_IN,
			IS_GREATER_OR_EQUAL,
			IS_GREATER,
			IS,
			IS_NOT
		}
		
		/**
		 * Constructor.
		 * 
		 */
		public SingleValuedAttributePredicate(
		) {
			super();
		}
		
		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public SingleValuedAttributePredicate(
			String id,
			String description,
			String qualifiedFeatureName
		) {
	        super(
	        	id, 
	        	description,
	        	qualifiedFeatureName
	        );
        }

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param description
		 * @param qualifiedFeatureName
		 * @param condition
		 * @param value
		 */
		public SingleValuedAttributePredicate(
			String id,
			String description,
			String qualifiedFeatureName,
			Condition condition,
			String value
		) {
	        super(
	        	id, 
	        	description,
	        	qualifiedFeatureName
	        );
	        this.condition = condition;
	        this.value = value;
        }

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param description
		 * @param function
		 * @param qualifiedFeatureName
		 * @param condition
		 * @param value
		 */
		public SingleValuedAttributePredicate(
			String id,
			String description,
			String function,
			String qualifiedFeatureName,
			Condition condition,
			String value
		) {
	        super(
	        	id, 
	        	description,
	        	qualifiedFeatureName
	        );
	        this.function = function;
	        this.condition = condition;
	        this.value = value;
        }

		/**
		 * Map condition to SQL operator.
		 * 
		 * @param condition
		 * @return
		 */
		public String conditionToSqlOperator(
			Condition condition
		) {
			switch(condition) {
				case IS_LIKE: return "LIKE";
				case IS_UNLIKE: return "NOT LIKE";
				case IS_LESS_OR_EQUAL: return "<=";
				case IS_LESS: return "<";
				case IS_NOT_IN: return "NOT IN";
				case IS_IN: return "IN";
				case IS_GREATER_OR_EQUAL: return ">=";
				case IS_GREATER: return ">";
				case IS: return "IS";
				case IS_NOT: return "IS NOT";
			}
			return "?";
		}

		/**
		 * @return the condition
		 */
		public Condition getCondition() {
			return condition;
		}
		
		/**
		 * @param condition the condition to set
		 */
		public void setCondition(Condition condition) {
			this.condition = condition;
		}
		
		/**
		 * @return the value
		 */
		public String getValue() {
			return value;
		}
		
		/**
		 * @param value the value to set
		 */
		public void setValue(String value) {
			this.value = value;
		}

		/**
		 * @return the function
		 */
		public String getFunction() {
			return function;
		}

		/**
		 * @param function the function to set
		 */
		public void setFunction(String function) {
			this.function = function;
		}

		/* (non-Javadoc)
		 * @see org.opencrx.kernel.portal.wizard.QueryBuilderController.Predicate#toClause(java.lang.String, org.openmdx.base.naming.Path, java.lang.String)
		 */
		@Override
        public String toSql(
        	String indent,
        	Path accessPath,
        	String tableAlias
        ) throws ServiceException {
			String columnName = this.getColumnName();
			String clause = "";
			if(this.getDescription() != null) {
				clause += indent + "/* " + this.getDescription() + " */\n";
			}
			clause += 
				indent + "(" + 
				(this.function == null 
					? tableAlias + "." + columnName 
					: this.function.replace((String)this.getFeature().getName(), tableAlias + "." + columnName)
				) + 
				" " +
				this.conditionToSqlOperator(this.condition) + " " + this.value + 
				")";
			return clause;
        }

		protected String function;
		protected Condition condition;
		protected String value;

	}

	/**
	 * MultiValuedAttributePredicate.
	 *
	 */
	public static class MultiValuedAttributePredicate extends FeaturePredicate {
	
		/**
		 * Condition
		 *
		 */
		public enum Condition {
			IS_IN,
			IS_NOT_IN,
			IS_EMPTY,
			IS_NOT_EMPTY
		}

		/**
		 * Constructor.
		 * 
		 */
		public MultiValuedAttributePredicate(
		) {
			super();
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public MultiValuedAttributePredicate(
			String id,
			String description,
			String qualifiedFeatureName
		) {
	        super(
	        	id, 
	        	description,
	        	qualifiedFeatureName
	        );
        }

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public MultiValuedAttributePredicate(
			String id,
			String description,
			String qualifiedFeatureName,
			Condition condition,
			String values
		) {
	        super(
	        	id, 
	        	description,
	        	qualifiedFeatureName
	        );
	        this.condition = condition;
	        this.values = values;
        }

		/**
		 * @return the condition
		 */
		public Condition getCondition() {
			return condition;
		}
		
		/**
		 * @param condition the condition to set
		 */
		public void setCondition(Condition condition) {
			this.condition = condition;
		}
		
		/**
		 * @return the values
		 */
		public String getValues() {
			return values;
		}
		
		/**
		 * @param values the values to set
		 */
		public void setValues(String values) {
			this.values = values;
		}

		/* (non-Javadoc)
		 * @see org.opencrx.kernel.portal.wizard.QueryBuilderController.Predicate#toClause(java.lang.String, org.openmdx.base.naming.Path, java.lang.String)
		 */
		@Override
        public String toSql(
        	String indent,
        	Path accessPath,
        	String tableAlias
        ) throws ServiceException {			
			String clause = "";
			if(this.getDescription() != null) {
				clause += indent + "/* " + this.getDescription() + " */\n";
			}
			Integer upperBound = this.getEmbeddedFeature();
			if(upperBound == null) {
				String columnName = this.getColumnName();
				Model_1_0 model = Model_1Factory.getModel();
				DbObject dbObject = this.getDbObject(
					this.getReferencedTypeAccessPath(
						model.getReferenceType(accessPath),
						accessPath
					)
				);
				String tableName_ = dbObject.getConfiguration().getDbObjectForUpdate2() == null
					? dbObject.getConfiguration().getDbObjectForQuery2()
					: dbObject.getConfiguration().getDbObjectForUpdate2();
				switch(this.condition) {
					case IS_IN:
						clause += indent + "EXISTS (SELECT 0 FROM " + tableName_ + " " + tableAlias + "_ WHERE " + tableAlias + ".object_id = " + tableAlias + "_.object_id AND " + columnName + " IN " + this.values + ")";
						break;
					case IS_NOT_IN:
						clause += indent + "NOT EXISTS (SELECT 0 FROM " + tableName_ + " " + tableAlias + "_ WHERE " + tableAlias + ".object_id = " + tableAlias + "_.object_id AND " + columnName + " IN " + this.values + ")";
						break;
					case IS_EMPTY:
						clause += indent + "NOT EXISTS (SELECT 0 FROM " + tableName_ + " " + tableAlias + "_ WHERE " + tableAlias + ".object_id = " + tableAlias + "_.object_id AND " + columnName + " IS NOT NULL)";
						break;
					case IS_NOT_EMPTY:
						clause += indent + "EXISTS (SELECT 0 FROM " + tableName_ + " " + tableAlias + "_ WHERE " + tableAlias + ".object_id = " + tableAlias + "_.object_id AND " + columnName + " IS NOT NULL)";
						break;
				}
			} else {
				clause += indent + "(";
				switch(this.condition) {
					case IS_IN:
						for(int index = 0; index < upperBound; index++) {
							String columnName = this.getColumnName(index);
							if(index > 0) {
								clause += " OR ";
							}
							clause += tableAlias + "." + columnName + " IN " + this.values;
						}
						break;
					case IS_NOT_IN:
						for(int index = 0; index < upperBound; index++) {
							String columnName = this.getColumnName(index);
							if(index > 0) {
								clause += " AND ";
							}							
							clause += tableAlias + "." + columnName + " NOT IN " + this.values;
						}
						break;
					case IS_EMPTY:
						for(int index = 0; index < upperBound; index++) {
							String columnName = this.getColumnName(index);
							if(index > 0) {
								clause += " AND ";
							}
							clause += tableAlias + "." + columnName + " IS NULL";						
						}
						break;
					case IS_NOT_EMPTY:
						for(int index = 0; index < upperBound; index++) {
							String columnName = this.getColumnName(index);
							if(index > 0) {
								clause += " OR ";
							}		
							clause += tableAlias + "." + columnName + " IS NOT NULL";						
						}
						break;
				}
				clause += ")";
			}
	        return clause;
        }

		protected Condition condition;
		protected String values;

	}

	/**
	 * ReferencePredicate.
	 *
	 */
	public static class ReferencePredicate extends FeaturePredicate {
		
		/**
		 * Condition
		 *
		 */
		public enum Condition {
			EXISTS,
			NOT_EXISTS
		}
		
		/**
		 * Constructor.
		 * 
		 */
		public ReferencePredicate(
		) {
			super();
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public ReferencePredicate(
			String id,
			String description,
			String qualifiedFeatureName
		) {
	        super(
	        	id, 
	        	description,
	        	qualifiedFeatureName
	        );
        }

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public ReferencePredicate(
			String id,
			String description,
			String qualifiedFeatureName,
			Condition condition,
			Predicate predicate
		) {
	        super(
	        	id, 
	        	description, 
	        	qualifiedFeatureName
	        );
	        this.condition = condition;
	        this.predicate = predicate;
        }

		/* (non-Javadoc)
		 * @see org.opencrx.kernel.utils.QueryBuilderUtil.Predicate#findPredicate(java.lang.String)
		 */
		@Override
		public Predicate findPredicate(
			String id
		) {
			Predicate f = super.findPredicate(id);
			return f == null
				? this.predicate == null
					? null
					: this.predicate.findPredicate(id)
				: f;
		}
		
		/**
		 * @return the predicate
		 */
		public Predicate getPredicate() {
			return predicate;
		}
		
		/**
		 * @param predicate the predicate to set
		 */
		public void setPredicate(Predicate predicate) {
			this.predicate = predicate;
		}
		
		/**
		 * @return the condition
		 */
		public Condition getCondition() {
			return condition;
		}
		
		/**
		 * @param condition the condition to set
		 */
		public void setCondition(Condition condition) {
			this.condition = condition;
		}

		/**
		 * @return the havingClause
		 */
		public String getHavingClause() {
			return havingClause;
		}

		/**
		 * @param havingClause the havingClause to set
		 */
		public ReferencePredicate setHavingClause(String havingClause) {
			this.havingClause = havingClause;
			return this;
		}

		/* (non-Javadoc)
		 * @see org.opencrx.kernel.portal.wizard.QueryBuilderController.Predicate#toClause(java.lang.String, org.openmdx.base.naming.Path, java.lang.String)
		 */
		@Override
        public String toSql(
        	String indent, 
        	Path accessPath,
        	String tableAlias
        ) throws ServiceException {
			ModelElement_1_0 feature = this.getFeature();
			String clause = "";
			if(this.getDescription() != null) {
				clause += indent + "/* " + this.getDescription() + " */\n";
			}
			switch(this.condition) {
				case EXISTS: 
					clause += indent + "EXISTS (\n";
					break;
				case NOT_EXISTS:
					clause += indent + "NOT EXISTS (\n";
					break;
			}
			clause += indent + TAB + "SELECT 0 FROM ";
			String joinClause = null;
			Path referencedTypeAccessPath = null;
			DbObject referencedDbObject = null;
			if(ModelHelper.isCompositeEnd(feature, false)) {
				referencedTypeAccessPath = this.getReferencedTypeAccessPath(feature, accessPath);
				referencedDbObject = this.getDbObject(referencedTypeAccessPath);
				joinClause = 
					"WHERE\n" + 
					indent + TAB + TAB + tableAlias + "v.p$$parent = " + tableAlias + ".object_id";
			} else if(ModelHelper.isSharedEnd(feature, false)) {
				referencedTypeAccessPath = this.getReferencedTypeAccessPath(feature, accessPath);
				referencedDbObject = this.getDbObject(referencedTypeAccessPath);
				String[] joinCriteria = referencedDbObject.getJoinCriteria();
				joinClause +=
					"\n" +
					indent + TAB + "INNER JOIN\n" + 
					indent + TAB + TAB + joinCriteria[0] + " " + tableAlias + "vj\n" + 
					indent + TAB + "ON\n" + 
					indent + TAB + TAB + tableAlias + "vj." + joinCriteria[2] + " = " + tableAlias + "v.object_id\n" + 
					indent + TAB + "WHERE\n" + 
					indent + TAB + TAB + tableAlias + "vj." + joinCriteria[1] + " = " + tableAlias + ".object_id";
			} else if(feature.isReference() && ModelHelper.isStoredAsAttribute(feature)) {
				referencedTypeAccessPath = this.getReferencedTypeAccessPath(feature, accessPath);
				referencedDbObject = this.getDbObject(referencedTypeAccessPath);
				String columnName = this.getColumnName();				
				if(ModelHelper.getMultiplicity(feature).isMultiValued()) {
					DbObject dbObject = this.getDbObject(accessPath);
					String tableName_ = dbObject.getConfiguration().getDbObjectForUpdate2() == null 
						? dbObject.getConfiguration().getDbObjectForQuery2() 
						: dbObject.getConfiguration().getDbObjectForUpdate2();
					joinClause =
						"WHERE\n" +
						indent + TAB + TAB + "EXISTS (SELECT 0 FROM " + tableName_ + " " + tableAlias + "_ WHERE " + tableAlias + "_.object_id = " + tableAlias + ".object_id AND " + tableAlias + "v.object_id = " + tableAlias +  "_." + columnName + ")";					
				} else {
					joinClause =
						"WHERE\n" + 
						indent + TAB + TAB + tableAlias + "v.object_id = " + tableAlias +  "." + columnName;
				}
			}
			if(referencedDbObject == null) {
				throw new ServiceException(
	        		BasicException.Code.DEFAULT_DOMAIN,
	        		BasicException.Code.NOT_FOUND,
	        		"No db object found for referencedTypeAccessPath",
	        		new BasicException.Parameter("referencedTypeAccessPath", referencedTypeAccessPath),
	        		new BasicException.Parameter("feature", feature),
	        		new BasicException.Parameter("accessPath", accessPath)
				);
			}
			String referencedTableName = referencedDbObject.getConfiguration().getDbObjectForUpdate1() == null 
				? referencedDbObject.getConfiguration().getDbObjectForQuery1() 
				: referencedDbObject.getConfiguration().getDbObjectForUpdate1();
			clause += 
				referencedTableName + 
				" " + tableAlias + "v " +
				joinClause + " AND\n";
			clause += this.predicate.toSql(
				indent + TAB + TAB, 
				referencedTypeAccessPath.size() % 2 == 1 ? referencedTypeAccessPath.getParent() : referencedTypeAccessPath, 
				tableAlias + "v"
			);
			clause += "\n";
	        if(this.havingClause != null) {
	        	clause += indent + TAB + "HAVING " + this.havingClause + "\n";
	        }			
			clause += indent + ")";
			return clause;
        }

		protected Condition condition;
		protected Predicate predicate;
		protected String havingClause;

	}

	/**
	 * TypeReferencePredicate
	 *
	 */
	public static class TypedReferencePredicate extends ReferencePredicate {
		
		/**
		 * Constructor.
		 * 
		 */
		public TypedReferencePredicate(
		) {
			super();
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public TypedReferencePredicate(
			String id,
			String description,
			String qualifiedFeatureName
		) {
	        super(
	        	id, 
	        	description,
	        	qualifiedFeatureName
	        );
        }

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public TypedReferencePredicate(
			String id,
			String description,
			String qualifiedFeatureName,
			String referencedTypeName,
			Condition condition,
			Predicate predicate
		) {
	        super(
	        	id, 
	        	description, 
	        	qualifiedFeatureName
	        );
	        this.referencedTypeName = referencedTypeName;
	        this.condition = condition;
	        this.predicate = predicate;
        }		
		
		/* (non-Javadoc)
		 * @see org.opencrx.kernel.utils.QueryBuilderUtil.FeaturePredicate#getReferencedTypeAccessPath(org.openmdx.base.mof.cci.ModelElement_1_0, org.openmdx.base.naming.Path)
		 */
		@Override
		protected Path getReferencedTypeAccessPath(
			ModelElement_1_0 reference,
			Path accessPath
		) throws ServiceException {			
			if(
				this.referencedTypeName != null && 
				reference.objGetValue("qualifiedFeatureName").equals(this.getQualifiedFeatureName())
			) {
				Model_1_0 model = reference.getModel();
				return model.getIdentityPattern(
					model.getElement(this.referencedTypeName)
				);
			} else {
				return super.getReferencedTypeAccessPath(reference, accessPath);
			}
		}

		/**
		 * @return the referencedTypeName
		 */
		public String getReferencedTypeName() {
			return referencedTypeName;
		}

		/**
		 * @param referencedTypeName the referencedTypeName to set
		 */
		public void setReferencedTypeName(String referencedTypeName) {
			this.referencedTypeName = referencedTypeName;
		}
		
		private String referencedTypeName;

	}

	/**
	 * CompositeParentPredicate.
	 *
	 */
	public static class CompositeParentPredicate extends FeaturePredicate {
		
		/**
		 * Condition
		 *
		 */
		public enum Condition {
			EXISTS,
			NOT_EXISTS
		}
		
		/**
		 * Constructor.
		 * 
		 */
		public CompositeParentPredicate(
		) {
			super();
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public CompositeParentPredicate(
			String id,
			String description
		) {
	        super(
	        	id, 
	        	description,
	        	null // qualifiedFeatureName
	        );
        }

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public CompositeParentPredicate(
			String id,
			String description,
			Condition condition,
			Predicate predicate
		) {
	        super(
	        	id, 
	        	description,
	        	null // qualifiedFeatureName
	        );
	        this.condition = condition;
	        this.predicate = predicate;
        }

		/* (non-Javadoc)
		 * @see org.opencrx.kernel.utils.QueryBuilderUtil.Predicate#findPredicate(java.lang.String)
		 */
		@Override
		public Predicate findPredicate(
			String id
		) {
			Predicate f = super.findPredicate(id);
			return f == null
				? this.predicate == null
					? null
					: this.predicate.findPredicate(id)
				: f;
		}

		/**
		 * @return the predicate
		 */
		public Predicate getPredicate() {
			return predicate;
		}
		
		/**
		 * @param predicate the predicate to set
		 */
		public void setPredicate(Predicate predicate) {
			this.predicate = predicate;
		}
		
		/**
		 * @return the condition
		 */
		public Condition getCondition() {
			return condition;
		}
		
		/**
		 * @param condition the condition to set
		 */
		public void setCondition(Condition condition) {
			this.condition = condition;
		}

		/**
		 * @return the havingClause
		 */
		public String getHavingClause() {
			return havingClause;
		}

		/**
		 * @param havingClause the havingClause to set
		 */
		public CompositeParentPredicate setHavingClause(String havingClause) {
			this.havingClause = havingClause;
			return this;
		}

		/* (non-Javadoc)
		 * @see org.opencrx.kernel.portal.wizard.QueryBuilderController.Predicate#toClause(java.lang.String, org.openmdx.base.naming.Path, java.lang.String)
		 */
		@Override
        public String toSql(
        	String indent, 
        	Path accessPath,
        	String tableAlias
        ) throws ServiceException {
			Model_1_0 model = Model_1Factory.getModel();
			String clause = "";
			if(this.getDescription() != null) {
				clause += indent + "/* " + this.getDescription() + " */\n";
			}
			switch(this.condition) {
				case EXISTS: 
					clause += indent + "EXISTS (\n";
					break;
				case NOT_EXISTS:
					clause += indent + "NOT EXISTS (\n";
					break;
			}
			clause += indent + TAB + "SELECT 0 FROM ";
			// Get access path of composite parent
			Path referencedTypeAccessPath = model.getIdentityPattern(
				model.getElement(model.getReferenceType(accessPath).getType())
			).getParent().getParent();
			DbObject referencedDbObject = this.getDbObject(referencedTypeAccessPath);
			String joinClause = 
				"WHERE\n" + 
				indent + TAB + TAB + tableAlias + ".p$$parent = " + tableAlias + "v.object_id";
			String referencedTableName = referencedDbObject.getConfiguration().getDbObjectForUpdate1() == null 
				? referencedDbObject.getConfiguration().getDbObjectForQuery1() 
				: referencedDbObject.getConfiguration().getDbObjectForUpdate1();
			clause += 
				referencedTableName + 
				" " + tableAlias + "v " +
				joinClause + " AND\n";
			clause += this.predicate.toSql(
				indent + TAB + TAB, 
				referencedTypeAccessPath.size() % 2 == 1 ? referencedTypeAccessPath.getParent() : referencedTypeAccessPath, 
				tableAlias + "v"
			);
			clause += "\n";
	        if(this.havingClause != null) {
	        	clause += indent + TAB + "HAVING " + this.havingClause + "\n";
	        }			
			clause += indent + ")";
			return clause;
        }

		protected Condition condition;
		protected Predicate predicate;
		protected String havingClause;

	}

	/**
	 * SelfJoinPredicate.
	 *
	 */
	public static class SelfJoinPredicate extends SingleValuedAttributePredicate {
		
		/**
		 * Constructor.
		 * 
		 */
		public SelfJoinPredicate(
		) {
			super();
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param description
		 * @param qualifiedFeatureName
		 */
		public SelfJoinPredicate(
			String id,
			String description,
			String qualifiedFeatureName
		) {
	        super(
	        	id, 
	        	description,
	        	qualifiedFeatureName
	        );
        }

		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param description
		 * @param qualifiedFeatureName
		 * @param condition
		 * @param predicate
		 */
		public SelfJoinPredicate(
			String id,
			String description,
			String qualifiedFeatureName,
			Condition condition,
			Predicate predicate
		) {
	        super(
	        	id, 
	        	description, 
	        	qualifiedFeatureName
	        );
	        this.condition = condition;
	        this.predicate = predicate;
        }

		/* (non-Javadoc)
		 * @see org.opencrx.kernel.utils.QueryBuilderUtil.Predicate#findPredicate(java.lang.String)
		 */
		@Override
		public Predicate findPredicate(
			String id
		) {
			Predicate f = super.findPredicate(id);
			return f == null
				? this.predicate == null
					? null
					: this.predicate.findPredicate(id)
				: f;
		}

		/**
		 * @return the predicate
		 */
		public Predicate getPredicate() {
			return predicate;
		}
		
		/**
		 * @param predicate the predicate to set
		 */
		public void setPredicate(Predicate predicate) {
			this.predicate = predicate;
		}
		
		/**
		 * @return the havingClause
		 */
		public String getHavingClause() {
			return havingClause;
		}
		
		/**
		 * @param havingClause the havingClause to set
		 */
		public SelfJoinPredicate setHavingClause(String havingClause) {
			this.havingClause = havingClause;
			return this;
		}
		
		/* (non-Javadoc)
		 * @see org.opencrx.kernel.portal.wizard.QueryBuilderController.Predicate#toClause(java.lang.String, org.openmdx.base.naming.Path, java.lang.String)
		 */
		@Override
        public String toSql(
        	String indent, 
        	Path accessPath,
        	String tableAlias
        ) throws ServiceException {
			String clause = "";
			if(this.getDescription() != null) {
				clause += indent + "/* " + this.getDescription() + " */\n";
			}
			String columnName = this.getColumnName();
			clause += indent + tableAlias + "." + columnName + " " + this.conditionToSqlOperator(this.condition) + " (\n";
			clause += indent + TAB + "SELECT DISTINCT(" + tableAlias + "v." + columnName + ") FROM ";
			DbObject dbObject = this.getDbObject(accessPath);
			String tableName = dbObject.getConfiguration().getDbObjectForUpdate1() == null 
				? dbObject.getConfiguration().getDbObjectForQuery1() 
				: dbObject.getConfiguration().getDbObjectForUpdate1();
			clause += 
				tableName + " " +
				tableAlias + "v " +
				"WHERE\n";
			clause += this.predicate.toSql(
				indent + TAB + TAB, 
				accessPath.getParent(), 
				tableAlias + "v"
			);
			clause += "\n";
	        if(this.havingClause != null) {
	        	clause += indent + TAB + "GROUP BY (" + tableAlias + "v." + columnName + ") HAVING " + this.havingClause + "\n";
	        }			
			clause += indent + ")";
			return clause;
        }

		protected Condition condition;
		protected Predicate predicate;
		protected String havingClause;

	}

	/**
	 * OwnerPredicate.
	 *
	 */
	public static class OwnerPredicate extends FeaturePredicate {
		
		/**
		 * Condition
		 *
		 */
		public enum Condition {
			EXISTS,
			NOT_EXISTS
		}
		
		/**
		 * Constructor.
		 * 
		 */
		public OwnerPredicate(
		) {
			super();
		}

		/**
		 * Constructor.
		 * 
		 * @param id
		 */
		public OwnerPredicate(
			String id,
			String description
		) {
	        super(
	        	id, 
	        	description, 
	        	"org:opencrx:kernel:base:SecureObject:owner"
	        );
        }
		
		/**
		 * Constructor.
		 * 
		 * @param id
		 * @param databasePlugIn
		 */
		public OwnerPredicate(
			String id,
			String description,
			Condition condition,
			Predicate predicate
		) {
	        super(
	        	id, 
	        	description,
	        	"org:opencrx:kernel:base:SecureObject:owner"
	        );
	        this.condition = condition;
	        this.predicate = predicate;
        }
		
		/* (non-Javadoc)
		 * @see org.opencrx.kernel.utils.QueryBuilderUtil.Predicate#findPredicate(java.lang.String)
		 */
		@Override
		public Predicate findPredicate(
			String id
		) {
			Predicate f = super.findPredicate(id);
			return f == null
				? this.predicate == null
					? null
					: this.predicate.findPredicate(id)
				: f;
		}

		/**
		 * @return the predicate
		 */
		public Predicate getPredicate() {
			return predicate;
		}
		
		/**
		 * @param predicate the predicate to set
		 */
		public void setPredicate(Predicate predicate) {
			this.predicate = predicate;
		}
		
		/**
		 * @return the condition
		 */
		public Condition getCondition() {
			return condition;
		}
		
		/**
		 * @param condition the condition to set
		 */
		public void setCondition(Condition condition) {
			this.condition = condition;
		}
		
		/* (non-Javadoc)
		 * @see org.opencrx.kernel.portal.wizard.QueryBuilderController.Predicate#toClause(java.lang.String, org.openmdx.base.naming.Path, java.lang.String)
		 */
		@Override
        public String toSql(
        	String indent, 
        	Path accessPath,
        	String tableAlias
        ) throws ServiceException {
			DbObject dbObject = this.getDbObject(accessPath);
			String tableName_ = dbObject.getConfiguration().getDbObjectForUpdate2() == null
				? dbObject.getConfiguration().getDbObjectForQuery2()
				: dbObject.getConfiguration().getDbObjectForUpdate2();
			// Join with principals
			Path principalsAccessPath = new Path("xri://@openmdx*org.openmdx.security.realm1").getDescendant("provider", ":*", "segment", ":*", "realm", ":*", "principal", ":*");
			DbObject dbObjectPrincipals = this.getDbObject(principalsAccessPath);
			String tableNamePrincipals = dbObjectPrincipals.getConfiguration().getDbObjectForUpdate1() == null
				? dbObjectPrincipals.getConfiguration().getDbObjectForQuery1()
				: dbObjectPrincipals.getConfiguration().getDbObjectForUpdate1();
			String clause = "";
			if(this.getDescription() != null) {
				clause += indent + "/* " + this.getDescription() + " */\n";
			}
			switch(this.condition) {
				case EXISTS:
					clause += indent + "EXISTS (\n";
					break;
				case NOT_EXISTS:
					clause += indent + "NOT EXISTS (\n";
					break;
			}
			clause += 
				indent + TAB + "SELECT 0 FROM " + tableNamePrincipals + " " + tableAlias + "v\n" +
				indent + TAB + "INNER JOIN\n" +
				indent + TAB + TAB + tableName_ + " " + tableAlias + "_\n" +
				indent + TAB + "ON\n" + 
				indent + TAB + TAB + tableAlias + "v.object_id LIKE ('principal/%/Root/' || REPLACE(" + tableAlias + "_.owner, ':', '/'))\n" +
				indent + TAB + "WHERE\n" +
				indent + TAB + TAB + tableAlias + "_.object_id = " + tableAlias + ".object_id AND\n";
			clause += this.predicate.toSql(
				indent + TAB + TAB, 
				principalsAccessPath.getParent(), 
				tableAlias + "v"
			);
			clause += "\n";
			clause += indent + ")";
			return clause;
        }

		protected Condition condition;
		protected Predicate predicate;

	}
	
	protected static Database_1[] databasePlugIns;
}
