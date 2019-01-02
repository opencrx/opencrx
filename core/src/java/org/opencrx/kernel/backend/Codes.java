/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Codes
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2018, CRIXP Corp., Switzerland
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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.Query;
import javax.jmi.reflect.RefObject;

import org.oasisopen.jmi1.RefContainer;
import org.omg.mof.spi.Names;
import org.opencrx.kernel.code1.jmi1.BasicValidatorCondition;
import org.opencrx.kernel.code1.jmi1.ComplexValidatorCondition;
import org.opencrx.kernel.code1.jmi1.ObjectValidator;
import org.opencrx.kernel.code1.jmi1.SequenceBasedValueRange;
import org.opencrx.kernel.code1.jmi1.ValidateObjectResult;
import org.opencrx.kernel.code1.jmi1.ValidatorCondition;
import org.opencrx.kernel.code1.jmi1.ValueRange;
import org.opencrx.kernel.code1.jmi1.ValueRangeUpdateValuesResult;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.cci2.BasicObjectQuery;
import org.openmdx.base.cci2.ContextCapableQuery;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.jmi1.ContextCapable;
import org.openmdx.base.jmi1.ExtentCapable;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.persistence.cci.Queries;
import org.openmdx.base.query.ConditionType;
import org.openmdx.base.rest.spi.Facades;
import org.openmdx.base.rest.spi.Query_2Facade;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.loading.Classes;
import org.w3c.cci2.AnyTypePredicate;
import org.w3c.format.DateTimeFormat;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * Codes
 *
 */
public class Codes extends AbstractImpl {

	/**
	 * Register Codes backend instance.
	 * 
	 */	
	public static void register(
	) {
		registerImpl(new Codes());
	}
	
	/**
	 * Get Codes backend instance.
	 * 
	 * @return
	 * @throws ServiceException
	 */	
	public static Codes getInstance(
	) throws ServiceException {
		return getInstance(Codes.class);
	}

	/**
	 * Constructor.
	 * 
	 */
	protected Codes(
	) {
		
	}

    /**
     * Get code segment.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public org.opencrx.kernel.code1.jmi1.Segment getCodeSegment(
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        return (org.opencrx.kernel.code1.jmi1.Segment)pm.getObjectById(
            new Path("xri://@openmdx*org.opencrx.kernel.code1").getDescendant("provider", providerName, "segment", segmentName)
        );
    }

	/**
	 * ValueProvider
	 *
	 */
	public interface ValueRangeProvider {
		public int getNextValue();
	}

	/**
	 * SequenceBasedValueRangeProvider
	 *
	 */
	public class SequenceBasedValueRangeProvider implements ValueRangeProvider {
		
		public SequenceBasedValueRangeProvider(
			Integer startValue,
			Integer nextValue,
			Integer incrementValue
		) {
			this.nextValue = nextValue == null ? startValue : nextValue;
			this.incrementValue = incrementValue == null ? 1 : incrementValue;
		}
		
		public int getNextValue(
		) {
			int nextValue = this.nextValue;
			this.nextValue += this.incrementValue;
			return nextValue;
		}

		private int nextValue;
		private int incrementValue;
	}

	/**
	 * Get value provider for given value range. Override for custom-specific behavior.
	 *  
	 * @param valueRange
	 * @return
	 * @throws ServiceException
	 */
	public ValueRangeProvider getValueRangeProvider(
		ValueRange valueRange
	) throws ServiceException {
		if(valueRange instanceof SequenceBasedValueRange) {
			SequenceBasedValueRange sequenceBasedValueRange = (SequenceBasedValueRange)valueRange;
			return new SequenceBasedValueRangeProvider(
				sequenceBasedValueRange.getStartValue(),
				sequenceBasedValueRange.getNextValue(),
				sequenceBasedValueRange.getIncrementValue()
			);
		} else {
			throw new ServiceException(
				BasicException.Code.DEFAULT_DOMAIN,
				BasicException.Code.NOT_SUPPORTED,
				"no value range provider for given value range type",
				new BasicException.Parameter(
					"type",
					valueRange.getClass().getName()
				)
			);
		}
	}

	/**
	 * Get default query so that it selects objects to be updated for given valueRange.
	 * The default query is 'forAll{storagePath}.isNull();'.
	 * Override this method for custom-specific behavior.
	 * 
	 * @param valueRange
	 * @return
	 * @throws ServiceException
	 */
	public String getDefaultUpdateValuesQuery(
		ValueRange valueRange
	) throws ServiceException {
		return valueRange.getStoragePath() + "().isNull();";
	}

	/**
	 * New query so that selects objects to be updated for given valueRange.
	 * 
	 * @param query
	 * @throws ServiceException
	 */
	public Query newUpdateValuesQuery(
		ValueRange valueRange
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(valueRange);
		Query_2Facade queryFacade = Facades.newQuery(null);
		queryFacade.setQueryType(valueRange.getQueryType());
		queryFacade.setQuery(
			valueRange.getQuery() == null
				? this.getDefaultUpdateValuesQuery(valueRange)
				: valueRange.getQuery()
		);
		Query query = pm.newQuery(
    		org.openmdx.base.persistence.cci.Queries.QUERY_LANGUAGE, 
    		queryFacade.getDelegate()
        );
		// Apply extent
		try {
			String providerName = valueRange.refGetPath().getSegment(2).toClassicRepresentation();
			String segmentName = valueRange.refGetPath().getSegment(4).toClassicRepresentation();
			Path identityPattern = new Path(valueRange.getIdentityPattern());
			query.setCandidates(
				PersistenceHelper.getCandidates(
		    		pm.getExtent(
		    			Classes.getApplicationClass(
		    				Names.toClassName(
		    					valueRange.getQueryType(),
		    					Names.JMI1_PACKAGE_SUFFIX
		    				)
		    			)
		    		),
		    		new Path(
						new String[]{
							identityPattern.getSegment(0).toClassicRepresentation(),
							"provider",
							providerName,
							"segment",
							segmentName
						}
					).getDescendant(
		    			identityPattern.getSegments().subList(5, identityPattern.size())
		    		)
				)
			);
		} catch(Exception e) {
			new ServiceException(e);
		}
		// Restrict to objectTypes
		if(!valueRange.getObjectType().isEmpty()) {
			try {
				@SuppressWarnings("unchecked")
				Class<? extends RefObject>[] objectTypes = new Class[valueRange.getObjectType().size()];
				for(int i = 0; i < objectTypes.length; i++) {
					objectTypes[i] = Classes.getApplicationClass(
	    				Names.toClassName(
	    					valueRange.getObjectType().get(i),
	    					Names.JMI1_PACKAGE_SUFFIX
	    				)
		    		);
				}
				PersistenceHelper.setClasses((AnyTypePredicate)query, objectTypes);
			} catch(Exception e) {
				new ServiceException(e);
			}
		}		
		return query;
	}

	/**
	 * Get value for given feature. Override for custom-specific behavior.
	 * 
	 * @param object
	 * @param feature
	 * @param nextValue
	 * @param date
	 * @return
	 * @throws ServiceException
	 */
	public Object getValue(
		RefObject_1_0 object,
		String feature,
		ValueRangeProvider valueRangeProvider
	) throws ServiceException {
		Object value = null;
		if("$nextValue".equals(feature)) {
			value = valueRangeProvider.getNextValue();
		} else if("$date".equals(feature)) {
			value = new Date();
		} else {
			value = object.refGetValue(feature);
		}
		return value;
	}

	/**
	 * Set formatted value at given storagePath.
	 * Override this method for custom-specific behavior.
	 * 
	 * @param object
	 * @param storagePath
	 * @param format
	 * @param arguments 
	 * @param value
	 * @throws ServiceException
	 */
	public void updateValue(
		RefObject_1_0 object,
		ValueRange valueRange,
		ValueRangeProvider valueRangeProvider
	) throws ServiceException {
		List<String> arguments = valueRange.getArgument();
		Object[] args = new Object[arguments.size()];
		for(int i = 0; i < arguments.size(); i++) {
			String featurePath = arguments.get(i);
			Object value = object;
			for(String feature: featurePath.split("\\.")) {
				value = this.getValue(
					(RefObject_1_0)value,
					feature,
					valueRangeProvider
				);
			}
			args[i] = value;
		}
		object.refSetValue(
			valueRange.getStoragePath(),
			String.format(valueRange.getFormat(), args)
		);
	}

	/**
	 * updateValues() performs the following steps:
	 * <ul>
     *   <li>Retrieve all objects of the extent ordered by createdAt not having a value at ::storagePath
     *   <li>For each of these objects get a value from the value provider
     *   <li>Format the value and save it to object's ::storagePath
     *   <li>update nextValue
     * </ul>
	 * 
	 * @param valueRange
	 * @param batchSize
	 * @return
	 * @throws ServiceException
	 */
	public ValueRangeUpdateValuesResult updateValues(
		SequenceBasedValueRange valueRange,
		Integer batchSize
	) throws ServiceException {
		int numberProcessed = 0;
		String statusMessage = null;
		short statusCode = 0;
		boolean rollbackOnError = false;
		ServiceException se = null;
		try {
			PersistenceManager pm = JDOHelper.getPersistenceManager(valueRange);
			Path identityPattern = new Path(valueRange.getIdentityPattern());
			String providerName = valueRange.refGetPath().getSegment(2).toClassicRepresentation();
			String segmentName = valueRange.refGetPath().getSegment(4).toClassicRepresentation();
			if(batchSize == null) {
				batchSize = DEFAULT_BATCH_SIZE;
			}
			Path segmentIdentity = new Path(
				new String[]{
					identityPattern.getSegment(0).toClassicRepresentation(),
					"provider",
					providerName,
					"segment",
					segmentName
				}
			);
			org.opencrx.kernel.jmi1.Segment segment = (org.opencrx.kernel.jmi1.Segment)pm.getObjectById(segmentIdentity);
			Query query = this.newUpdateValuesQuery(valueRange);
    		if(query instanceof BasicObjectQuery) {
    			BasicObjectQuery basicObjectQuery = (BasicObjectQuery)query;
    			basicObjectQuery.orderByCreatedAt().ascending();
    			// Restrict to candidates not having a value
    			if(query != null) {
	    			List<BasicObject> objects = segment.getExtent(basicObjectQuery);
	    			ValueRangeProvider valueRangeProvider = this.getValueRangeProvider(valueRange);
	    			statusMessage = "{\"statusMessage\":[";
	    			String separator = "";
	    			for(BasicObject object: objects) {
	    				this.updateValue(
	    					object,
	    					valueRange,
	    					valueRangeProvider
	    				);
	    				statusMessage += separator + "{\"object\": \"" + object.refGetPath().toXRI() + "\"}";
	    				separator = ",";
	    				rollbackOnError = true;
	    				numberProcessed++;
	    				if(numberProcessed >= batchSize) break;
	    			}
	    			statusMessage += "]}";
	    			valueRange.setNextValue(valueRangeProvider.getNextValue());
	    			rollbackOnError = true;
    			}
    		}
		} catch(Exception e) {
			se = new ServiceException(e);
		}
		if(rollbackOnError && se != null) {
			throw se;
		} else {
			if(se != null) {
				statusMessage = se.getMessage();
				statusCode = (short)se.getExceptionCode();
			}
			return Structures.create(
				ValueRangeUpdateValuesResult.class, 
				Datatypes.member(ValueRangeUpdateValuesResult.Member.statusMessage, statusMessage),
				Datatypes.member(ValueRangeUpdateValuesResult.Member.statusCode, statusCode),			
				Datatypes.member(ValueRangeUpdateValuesResult.Member.numberProcessed, numberProcessed)
			);
		}
	}

	/**
	 * Convert value to BigDecimal.
	 * 
	 * @param value
	 * @return
	 */
	protected BigDecimal toBigDecimal(
		Object value
	) {
		if(value == null) {
			return null;
		} else {
			return new BigDecimal(value.toString());
		}
	}

	/**
	 * Return true if object matches condition.
	 * 
	 * @param condition
	 * @param object
	 * @param validationTime
	 * @return
	 * @throws ServiceException
	 */
	public Boolean evaluateCondition(
		ValidatorCondition condition,
		ContextCapable object,
		Date validationTime,
		List<String> selectors,
		List<org.opencrx.kernel.code1.cci2.ValidatorCondition> includes,
		List<org.opencrx.kernel.code1.cci2.ValidatorCondition> excludes,
		StringBuffer statusMessage
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(condition);
		Boolean result = null;
		String comment = "";
		if(condition instanceof ComplexValidatorCondition) {
			ComplexValidatorCondition complexCondition = (ComplexValidatorCondition)condition;
			List<Boolean> values = new ArrayList<Boolean>();
			for(ValidatorCondition operand: complexCondition.<ValidatorCondition>getOperand()) {
				if(
					!Boolean.TRUE.equals(condition.isDisabled()) &&
					(validationTime == null || operand.getValidFrom() == null || validationTime.compareTo(operand.getValidFrom()) > 0) &&
					(validationTime == null || operand.getValidTo() == null || validationTime.compareTo(operand.getValidTo()) < 0) &&
					(selectors.isEmpty() || !Collections.disjoint(selectors,  operand.getSelector())) &&
					(includes.isEmpty() || includes.contains(operand)) &&
					(excludes.isEmpty() || !excludes.contains(operand))
				) {
					values.add(
						this.evaluateCondition(
							operand,
							object,
							validationTime,
							selectors,
							includes,
							excludes,
							statusMessage
						)
					);
				}
			}
			if(values.contains(null)) {
				result = null;
			} else if(complexCondition.getOperator() == BooleanOperator.OR.getValue()) {
				result = values.contains(Boolean.TRUE);
			} else if(complexCondition.getOperator() == BooleanOperator.AND.getValue()) {
				result = !values.contains(Boolean.FALSE);
			}
			comment = values.toString();
		} else if(condition instanceof BasicValidatorCondition) {
			BasicValidatorCondition basicCondition = (BasicValidatorCondition)condition;
			// Collect candidates
			List<ContextCapable> candidates = new ArrayList<ContextCapable>();
			if("this".equalsIgnoreCase(basicCondition.getObjectQuery())) {
				candidates.add(object);
			} else {
				String[] objectQuery = basicCondition.getObjectQuery().split("\\?");
				String referenceName = objectQuery[0];
				String queryType = null;
				String queryString = null;
				int size = DEFAULT_BATCH_SIZE;
				if(objectQuery.length > 1) {
					for(String nv: objectQuery[1].split("&")) {
						int pos = nv.indexOf("=");
						if(pos > 0) {
							String name = nv.substring(0, pos);
							String value = nv.substring(pos + 1);
							if("query".equals(name)) {
								queryString = value;
							} else if("queryType".equals(name)) {
								queryType = value;
							} else if("size".equals(name)) {
								size = Integer.valueOf(value);
							}
						}
					}
				}
				if(queryType == null || queryType.isEmpty()) {
					throw new ServiceException(
						BasicException.Code.DEFAULT_DOMAIN,
						BasicException.Code.ASSERTION_FAILURE,
						"Missing parameter queryType in objectQuery",
						new BasicException.Parameter("objectQuery", basicCondition.getObjectQuery())
					);
				}
				if(queryString == null || queryString.isEmpty()) {
					throw new ServiceException(
						BasicException.Code.DEFAULT_DOMAIN,
						BasicException.Code.ASSERTION_FAILURE,
						"Missing parameter queryString in objectQuery",
						new BasicException.Parameter("objectQuery", basicCondition.getObjectQuery())
					);
				}
				ContextCapableQuery query = null;
				{
					Query_2Facade queryFacade = Facades.newQuery(null);
					queryFacade.setQueryType(queryType);
					if(queryString != null) {
						queryFacade.setQuery(queryString);
					}
					query = (ContextCapableQuery)pm.newQuery(
			        	Queries.QUERY_LANGUAGE,
			        	queryFacade.getDelegate()
			        );					
				}
				int count = 0;
				for(ContextCapable candidate: ((RefContainer<ContextCapable>)object.refGetValue(referenceName)).refGetAll(query)) {
					candidates.add(candidate);
					count++;
					if(count >= size) break;
				}
			}
			// Collect values for all candidates for feature ::featurePath
			Map<RefObject_1_0,Object> valueMap = new HashMap<RefObject_1_0,Object>();
			for(ContextCapable candidate: candidates) {
				Object value = candidate;
				RefObject_1_0 lastObjectValue = null;
				for(String feature: basicCondition.getFeaturePath().split("\\.")) {
					lastObjectValue = (RefObject_1_0)value;
					value = ((RefObject_1_0)value).refGetValue(feature);
					if(value instanceof Collection) {
						Collection<?> values = (Collection<?>)value;
						value = values.isEmpty()
							? null
							: values.iterator().next();
					}
				}
				if(value instanceof RefObject_1_0) {
					lastObjectValue = (RefObject_1_0)value;
				}
				valueMap.put(
					lastObjectValue,
					value instanceof RefObject_1_0
						? ((RefObject_1_0)value).refGetPath()
						: value
				);
			}
			// Sort and filter by ::scope
			List<Object> values = new ArrayList<Object>();
			if(basicCondition.getScope().isEmpty()) {
				values.addAll(valueMap.values());
			} else {
				for(ExtentCapable scope: basicCondition.<ExtentCapable>getScope()) {
					if(valueMap.containsKey(scope)) {
						values.add(valueMap.get(scope));
					}
				}
			}
			// Apply aggregate
			Object aggregatedValue = null;
			comment = values.toString();
			if(basicCondition.getAggregateFunction() == AggregateFunction.SUM.getValue()) {
				aggregatedValue = BigDecimal.ZERO;
				for(Object value: values) {
					if(value != null) {
						aggregatedValue = ((BigDecimal)aggregatedValue).add(this.toBigDecimal(value));
					}
				}
			} else if(basicCondition.getAggregateFunction() == AggregateFunction.MIN.getValue()) {
				aggregatedValue = null;
				for(Object value: values) {
					if(aggregatedValue == null || value == null) {
						aggregatedValue = value;
					} else if(aggregatedValue instanceof Comparable && value instanceof Comparable) {
						if(((Comparable)aggregatedValue).compareTo((Comparable)value) < 0) {
							aggregatedValue = value;
						}
					} else {
						if(aggregatedValue.toString().compareTo(value.toString()) < 0) {
							aggregatedValue = value;
						}
					}
				}
			} else if(basicCondition.getAggregateFunction() == AggregateFunction.MAX.getValue()) {
				aggregatedValue = null;
				for(Object value: values) {
					if(aggregatedValue == null || value == null) {
						aggregatedValue = value;
					} else if(aggregatedValue instanceof Comparable && value instanceof Comparable) {
						if(((Comparable)aggregatedValue).compareTo((Comparable)value) > 0) {
							aggregatedValue = value;
						}
					} else {
						if(aggregatedValue.toString().compareTo(value.toString()) > 0) {
							aggregatedValue = value;
						}
					}
				}
			} else if(basicCondition.getAggregateFunction() == AggregateFunction.COUNT.getValue()) {
				aggregatedValue = values.size();
			} else if(basicCondition.getAggregateFunction() == AggregateFunction.TOSTRING.getValue()) {
				aggregatedValue = "";
				String sep = "";
				for(Object value: values) {
					if(value != null) {
						String stringifiedValue = null;
						if(value instanceof Date) {
							stringifiedValue = DateTimeFormat.BASIC_UTC_FORMAT.format((Date)value);
						} else if(value instanceof Path) {
							stringifiedValue = ((Path)value).getLastSegment().toClassicRepresentation();
						} else {
							stringifiedValue = value.toString();  
						}
						aggregatedValue = aggregatedValue + sep + stringifiedValue;
						sep = " ";
					}
				}
			}
			if(aggregatedValue == null) {
				aggregatedValue = "";
			} else if(aggregatedValue instanceof Date) {
				aggregatedValue = DateTimeFormat.BASIC_UTC_FORMAT.format((Date)aggregatedValue);
			}
			// Apply condition
			if(basicCondition.getCondition() == ConditionType.IS_IN.code()) {
				result = false;
				for(String argument: basicCondition.getConditionArgument()) {
					if(argument.equals(aggregatedValue.toString())) {
						result = true;
						break;
					}
				}
			} else if(basicCondition.getCondition() == ConditionType.IS_NOT_IN.code()) {
				result = true;
				for(String argument: basicCondition.getConditionArgument()) {
					if(argument.equals(aggregatedValue.toString())) {
						result = false;
						break;
					}
				}
			} else if(basicCondition.getCondition() == ConditionType.IS_LESS_OR_EQUAL.code()) {
				if(aggregatedValue instanceof Number) {
					result = this.toBigDecimal(aggregatedValue).compareTo(
						this.toBigDecimal(basicCondition.getConditionArgument().get(0))
					) <= 0;
				} else {
					result = aggregatedValue.toString().compareTo(
						basicCondition.getConditionArgument().get(0)
					) <= 0;
				}
			} else if(basicCondition.getCondition() == ConditionType.IS_LESS.code()) {
				if(aggregatedValue instanceof Number) {
					result = this.toBigDecimal(aggregatedValue).compareTo(
						this.toBigDecimal(basicCondition.getConditionArgument().get(0))
					) < 0;
				} else {
					result = aggregatedValue.toString().compareTo(
						basicCondition.getConditionArgument().get(0)
					) < 0;
				}
			} else if(basicCondition.getCondition() == ConditionType.IS_GREATER_OR_EQUAL.code()) {
				if(aggregatedValue instanceof Number) {
					result = this.toBigDecimal(aggregatedValue).compareTo(
						this.toBigDecimal(basicCondition.getConditionArgument().get(0))
					) >= 0;
				} else {
					result = aggregatedValue.toString().compareTo(
						basicCondition.getConditionArgument().get(0)
					) >= 0;
				}
			} else if(basicCondition.getCondition() == ConditionType.IS_BETWEEN.code()) {
				if(aggregatedValue instanceof Number) {
					result = this.toBigDecimal(aggregatedValue).compareTo(
						this.toBigDecimal(basicCondition.getConditionArgument().get(0))
					) >= 0 &
					this.toBigDecimal(aggregatedValue).compareTo(
						this.toBigDecimal(basicCondition.getConditionArgument().get(1))
					) <= 0;
				}
			} else if(basicCondition.getCondition() == ConditionType.IS_OUTSIDE.code()) {
				if(aggregatedValue instanceof Number) {
					result = this.toBigDecimal(aggregatedValue).compareTo(
						this.toBigDecimal(basicCondition.getConditionArgument().get(0))
					) < 0 ||
					this.toBigDecimal(aggregatedValue).compareTo(
						this.toBigDecimal(basicCondition.getConditionArgument().get(1))
					) > 0;
				}
			} else if(basicCondition.getCondition() == ConditionType.IS_GREATER.code()) {
				if(aggregatedValue instanceof Number) {
					result = this.toBigDecimal(aggregatedValue).compareTo(
						this.toBigDecimal(basicCondition.getConditionArgument().get(0))
					) > 0;
				} else {
					result = aggregatedValue.toString().compareTo(
						basicCondition.getConditionArgument().get(0)
					) > 0;
				}
			} else if(basicCondition.getCondition() == ConditionType.IS_LIKE.code()) {
				result = false;
				for(String argument: basicCondition.getConditionArgument()) {
					if(aggregatedValue.toString().matches(argument)) {
						result = true;
					}
				}
			} else if(basicCondition.getCondition() == ConditionType.IS_UNLIKE.code()) {
				result = true;
				for(String argument: basicCondition.getConditionArgument()) {
					if(aggregatedValue.toString().matches(argument)) {
						result = false;
					}
				}
			}
			comment += " → " + aggregatedValue.toString();
		}
		if(statusMessage.length() > 0) {
			statusMessage.append(", ");
		}
		statusMessage.append(
			"{" +
				"\"name\": \"" + condition.getName() + "\", " +
				"\"value\": " + result + ", " +
				"\"comment\": \"" + comment + "\"" +
			"}"
		);
		return result;
	}

	/**
	 * Validate given object.
	 * 
	 * @param objectValidator
	 * @param validationTime
	 * @return
	 * @throws ServiceException
	 */
	public ValidateObjectResult validateObject(
		ObjectValidator objectValidator,
		ContextCapable object,
		Date validationTime,
		List<String> selectors,
		List<org.opencrx.kernel.code1.cci2.ValidatorCondition> includes,
		List<org.opencrx.kernel.code1.cci2.ValidatorCondition> excludes
	) throws ServiceException {
		Boolean isValid = false;
		short statusCode = 0;
		StringBuffer validationResult = new StringBuffer("");
		if(objectValidator.getMainCondition() != null) {
			isValid = this.evaluateCondition(
				objectValidator.getMainCondition(),
				object,
				validationTime,
				selectors,
				includes,
				excludes,
				validationResult
			);
		}
		return Structures.create(
			ValidateObjectResult.class, 
			Datatypes.member(ValidateObjectResult.Member.isValid, isValid),
			Datatypes.member(ValidateObjectResult.Member.statusMessage, "{\"validationResult\": [" + validationResult.toString() + "]}"),
			Datatypes.member(ValidateObjectResult.Member.statusCode, statusCode)
		);
	}

    /**
     * AggregateFunction.
     * 
     */
    public enum AggregateFunction {
    	
    	NA((short)0),
    	COUNT((short)1),
    	SUM((short)2),
    	MIN((short)3),
    	MAX((short)4),
    	AVG((short)5),
    	TOSTRING((short)6);

		private short value;
		
		private AggregateFunction(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
		
    }
	
    /**
     * BooleanOperator.
     * 
     */
    public enum BooleanOperator {
    	
    	NA((short)0),
    	AND((short)1),
    	OR((short)2);
    	
		private short value;
		
		private BooleanOperator(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
		
    }
		
	//-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    public static final int DEFAULT_BATCH_SIZE = 50;
    
}
