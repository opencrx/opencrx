/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Exporter
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2011, CRIXP Corp., Switzerland
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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.StringTokenizer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFCellStyle;
import org.apache.poi.hssf.usermodel.HSSFName;
import org.apache.poi.hssf.usermodel.HSSFRichTextString;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.oasisopen.jmi1.RefContainer;
import org.opencrx.kernel.base.jmi1.ExportProfile;
import org.opencrx.kernel.document1.jmi1.DocumentRevision;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.openmdx.base.accessor.cci.SystemAttributes;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.io.QuotaByteArrayOutputStream;
import org.openmdx.base.mof.cci.AggregationKind;
import org.openmdx.base.mof.cci.ModelElement_1_0;
import org.openmdx.base.mof.cci.ModelHelper;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.mof.cci.Multiplicity;
import org.openmdx.base.mof.cci.PrimitiveTypes;
import org.openmdx.base.mof.spi.Model_1Factory;
import org.openmdx.base.naming.Path;
import org.openmdx.base.text.conversion.Base64;
import org.openmdx.base.text.conversion.XMLEncoder;
import org.openmdx.kernel.exception.BasicException;
import org.w3c.cci2.BinaryLargeObject;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.format.DateTimeFormat;

public class Exporter extends AbstractImpl {

	// -------------------------------------------------------------------------
	public static void register() {
		registerImpl(new Exporter());
	}

	// -------------------------------------------------------------------------
	public static Exporter getInstance() throws ServiceException {
		return getInstance(Exporter.class);
	}

	// -------------------------------------------------------------------------
	protected Exporter() {

	}

	// -------------------------------------------------------------------------
	public static class TraversedObject {

		public TraversedObject(RefObject_1_0 object, int level) {
			this.object = object;
			this.level = level;
		}

		public RefObject_1_0 getObject() {
			return this.object;
		}

		public int getLevel() {
			return this.level;
		}

		@Override
		public boolean equals(Object obj) {
			return obj instanceof TraversedObject ?
				this.object.equals(((TraversedObject)obj).object) :
				false;
		}

		private final RefObject_1_0 object;

		private final int level;
	}

	// -------------------------------------------------------------------------
	static class NullOutputStream extends OutputStream {

		public NullOutputStream() {
		}

		public void close() throws IOException {
		}

		public void flush() throws IOException {
		}

		public void write(byte[] b, int off, int len) throws IOException {
		}

		public void write(byte[] b) throws IOException {
		}

		public void write(int b) throws IOException {
		}
	}

	// -----------------------------------------------------------------------
	public static class ExportParams {

		public ExportParams() {
			this.referenceFilter = new HashSet<String>();
			this.options = new HashMap<String, List<String>>();
			this.context = new HashMap<String, Object>();
			this.currentEntryId = 0;
		}

		public Set<String> getReferenceFilter() {
			return this.referenceFilter;
		}

		public Map<String, List<String>> getOptions() {
			return this.options;
		}

		public String getMimeType() {
			return this.mimeType;
		}

		public Map<String, Object> getContext() {
			return this.context;
		}

		public void setMimeType(String mimeType) {
			this.mimeType = mimeType;
		}

		public int getNextEntryId() {
			return this.currentEntryId++;
		}

		public void resetEntryId() {
			this.currentEntryId = 0;
		}

		private final Set<String> referenceFilter;

		private final Map<String, List<String>> options;

		private final Map<String, Object> context;

		private String mimeType;

		private int currentEntryId;

	}

	static class XMLWriter {

		static private class Attribute {
			Attribute(String qName, String value) {
				_qName = qName;
				_value = value;
			}

			String _qName;

			String _value;
		}

		static public class Attributes {

			public Attributes() {
				_attributes = new ArrayList<Attribute>();
			}

			public void addCDATA(String qName, String value) {
				this._attributes.add(new Attribute(qName, value));
			}

			public String getQName(int index) {
				return this._attributes.get(index)._qName;

			}

			public String getValue(int index) {
				return this._attributes.get(index)._value;
			}

			public int getLength() {
				return this._attributes.size();
			}

			private List<Attribute> _attributes;
		}

		public XMLWriter(PrintStream out) {
			this.out = out;
			this.qNameStack = new Stack<String>();
		}

		public void startElement(String namespaceURI, String localName, String qName, Attributes atts) throws ServiceException {
			this.out.print('\n');
			for (int i = 0; i < this.qNameStack.size(); i++) {
				this.out.print(this.indentString);
			}
			this.out.print('<');
			this.out.print(qName);
			if(atts.getLength() > 0) {
				for (int i = 0; i < atts.getLength(); i++) {
					this.out.print(" ");
					this.out.print(XMLEncoder.encode(atts.getQName(i)));
					this.out.print('=');
					this.out.print('"');
					this.out.print(XMLEncoder.encode(atts.getValue(i)));
					this.out.print('"');
				}
			}
			this.out.write('>');
			this.qNameStack.push(qName);
		}

		public void endElement(String namespaceURI, String localName, String qName, boolean newLine) throws ServiceException {
			String expectedQName = this.qNameStack.pop();
			if(!expectedQName.equals(qName)) {
				throw new ServiceException(BasicException.Code.DEFAULT_DOMAIN, BasicException.Code.ASSERTION_FAILURE, "Non matching qName for XML tag.", new BasicException.Parameter("qName", qName),
				    new BasicException.Parameter("expected qName", expectedQName));
			}
			if(newLine) {
				this.out.print('\n');
				for (int i = 0; i < this.qNameStack.size(); i++) {
					this.out.print(this.indentString);
				}
			}
			this.out.print("</");
			this.out.print(XMLEncoder.encode(qName));
			this.out.print(">");
		}

		public void characters(char[] ch, int start, int length) throws ServiceException {
			String s = new String(ch, start, length);
			this.out.print(XMLEncoder.encode(s));
		}

		public void startDocument() throws ServiceException {
			this.out.print("<?xml version=\"1.0\" encoding=\"");
			this.out.print("UTF-8");
			this.out.print("\"?>");
		}

		public void comment(String comment) throws ServiceException {
			this.out.print("<!-- " + comment + " -->");
		}

		public void endDocument() throws ServiceException {
			this.out.flush();
			if(!this.qNameStack.isEmpty()) {
				throw new ServiceException(BasicException.Code.DEFAULT_DOMAIN, BasicException.Code.ASSERTION_FAILURE, "Open elements while endDocument().", new BasicException.Parameter("elements",
				    this.qNameStack.toString()));
			}
		}

		public void processingInstruction(String target, String data) throws ServiceException {
			this.out.print("\n<?");
			this.out.print(target);
			this.out.print(' ');
			this.out.print(data);
			this.out.print("?>");
		}

		private final PrintStream out;

		private String indentString = "\t";

		private final Stack<String> qNameStack;
	}

	// -------------------------------------------------------------------------
	public void exportItem(
	    OutputStream out,
	    PrintStream ps,
	    List<TraversedObject> startPoints,
	    List<TraversedObject> allExportedObjects,
	    List<TraversedObject> allReferencedObjects,
	    ExportParams params
	) throws IOException, ServiceException {
		if(!startPoints.isEmpty()) {
			List<TraversedObject> referencedObjects = new ArrayList<TraversedObject>();
			for (TraversedObject startPoint : startPoints) {
				if((startPoint.getLevel() < MAX_LEVELS) && !allExportedObjects.contains(startPoint)) {
					boolean isMultiFileExport = out instanceof ZipOutputStream;
					if(isMultiFileExport) {
						((ZipOutputStream) out).putNextEntry(new ZipEntry("data-" + (params.getNextEntryId()) + FILE_EXT_XML));
					}
					// derive schema name from identity
					String qualifiedModelName = startPoint.getObject().refGetPath().get(0);
					String namespace = qualifiedModelName.substring(0, qualifiedModelName.lastIndexOf(":"));
					String modelName = qualifiedModelName.substring(qualifiedModelName.lastIndexOf(":") + 1);
					ObjectExporter objectExporter = new ObjectExporter(
						ps, 
						params,
						allExportedObjects
					);
					objectExporter.export(
						startPoint, 
						startPoint.getObject().refGetPath().getParent().getBase(), 
						"xri://+resource/" + namespace.replace(':', '/') + "/" + modelName + "/xmi1/" + modelName + ".xsd"
					);
					// Update referenced and exported objects
					for (TraversedObject o : objectExporter.getReferencedObjects()) {
						if(!referencedObjects.contains(o)) {
							referencedObjects.add(o);
						}
						if(!allReferencedObjects.contains(o)) {
							allReferencedObjects.add(o);
						}
					}
				}
			}
			// Remove segment-level objects from set of referenced objects.
			// Otherwise we
			// might get very long-running exports
			for (Iterator<TraversedObject> i = referencedObjects.iterator(); i.hasNext();) {
				TraversedObject object = i.next();
				if(object.getObject().refGetPath().size() <= 5) {
					i.remove();
				}
			}
			this.exportItem(
				out, 
				ps, 
				referencedObjects, 
				allExportedObjects, 
				allReferencedObjects, 
				params
			);
		}
	}

	// -----------------------------------------------------------------------
	/**
	 * Export object according to the given export profile and itemMimeType. The
	 * default implementation allows to export objects in the format 
	 * application/xml or application/x-excel. The default implementation recursively walks
	 * the object (composites and references objects) according to the reference 
	 * filter. 
	 * Override this method for custom-specific export and rendering of objects. E.g.
	 * a custom-specific implementation could work as follows:
	 * <ul>
	 *   <li>Derive the document type / template from the export profile.
	 *   <li>Generate an XML according to the document type and object.
	 *   <li>Invoke a (remote) rendering engine with the parameters:
	 *       <ul>
	 *         <li>document type (or exportProfile.template if the document templates are not stored in the rendering engine)
	 *         <li>generated XML
	 *         <li>exportProfile.itemMimeType
	 *       </ul>
	 *   <li>Return the rendered object.
	 * </ul>
	 */
	public Object[] exportItem(
		RefObject_1_0 object, 
		ExportProfile exportProfile, 
		String referenceFilter, 
		String itemMimeType
	) throws ServiceException {
		try {
			PersistenceManager pm = JDOHelper.getPersistenceManager(exportProfile);
			if(exportProfile != null) {
				referenceFilter = exportProfile.getExportParams();
				itemMimeType = exportProfile.getMimeType();
			}
			ExportParams exportParams = new ExportParams();
			List<TraversedObject> roots = new ArrayList<TraversedObject>();
			roots.add(new TraversedObject(object, 1));
			if(referenceFilter != null) {
				// Starting identities are separated by $
				if(referenceFilter.indexOf("$") > 0) {
					StringTokenizer tokenizer = new StringTokenizer(referenceFilter.substring(0, referenceFilter.indexOf("$")), "\t\n ;,", false);
					while (tokenizer.hasMoreTokens()) {
						roots.add(
							new TraversedObject(
								(RefObject_1_0)pm.getObjectById(new Path(tokenizer.nextToken())), 
								1
							)
						);
					}
					referenceFilter = referenceFilter.substring(referenceFilter.indexOf("$") + 1);
				}
				// Options are separated by !
				String options = null;
				if(referenceFilter.indexOf("!") > 0) {
					options = referenceFilter.substring(referenceFilter.indexOf("!") + 1);
					referenceFilter = referenceFilter.substring(0, referenceFilter.indexOf("!"));
				}
				// Parse reference filter
				StringTokenizer referenceFilterTokenizer = new StringTokenizer(referenceFilter, "\t\n ;,", false);
				while (referenceFilterTokenizer.hasMoreTokens()) {
					String referenceName = referenceFilterTokenizer.nextToken();
					if(!referenceName.endsWith("]")) {
						referenceName += "[1]"; // by default maxLevel is 1
					}
					exportParams.getReferenceFilter().add(referenceName);
				}
				// Parse options. Options are of the format String[String;...],
				// String[String;...]
				if(options != null) {
					StringTokenizer optionsTokenizer = new StringTokenizer(options, "\t\n ,", false);
					while (optionsTokenizer.hasMoreTokens()) {
						String option = optionsTokenizer.nextToken();
						String optionName = null;
						List<String> optionParams = new ArrayList<String>();
						if(option.indexOf("[") > 0) {
							optionName = option.substring(0, option.indexOf("["));
							StringTokenizer optionParamsTokenizer = new StringTokenizer(option.substring(option.indexOf("[") + 1), ";]", false);
							while (optionParamsTokenizer.hasMoreTokens()) {
								optionParams.add(optionParamsTokenizer.nextToken());
							}
						}
						else {
							optionName = option;
						}
						exportParams.getOptions().put(optionName, optionParams);
					}
				}
			}
			exportParams.setMimeType(itemMimeType == null ? MIME_TYPE_XML : itemMimeType);
			List<TraversedObject> allExportedObjects = new ArrayList<TraversedObject>();
			List<TraversedObject> allReferencedObjects = new ArrayList<TraversedObject>();
			// 2-pass export
			// 1) determine transitive closure of objects to export
			// 2) export
			if((exportProfile != null) && (exportProfile.getTemplate() != null)) {
				exportParams.getContext().put("template", exportProfile.getTemplate());
			}
			boolean isMultiFileExport = exportParams.getMimeType().equals(MIME_TYPE_XML);
			// Pass 1
			OutputStream out = isMultiFileExport ? new ZipOutputStream(new NullOutputStream()) : new NullOutputStream();
			PrintStream ps = new PrintStream(out);
			this.exportItem(
				out, 
				ps, 
				roots, 
				allExportedObjects, 
				allReferencedObjects, 
				exportParams
			);
			// Prepare starting points. These are all referenced objects which
			// are not composite to either other referenced objects or exported
			// objects
			for (Iterator<TraversedObject> i = allReferencedObjects.iterator(); i.hasNext();) {
				TraversedObject pi = i.next();
				if(pi.getObject().refGetPath().size() <= 5) {
					i.remove();
				}
			}
			List<TraversedObject> nonCompositeStartingPoints = new ArrayList<TraversedObject>(allReferencedObjects);
			for (Iterator<TraversedObject> i = nonCompositeStartingPoints.iterator(); i.hasNext();) {
				TraversedObject pi = i.next();
				for (Iterator<TraversedObject> j = allReferencedObjects.iterator(); j.hasNext();) {
					TraversedObject pj = j.next();
					if(pi.getObject().refGetPath().size() > pj.getObject().refGetPath().size() && pi.getObject().refGetPath().startsWith(pj.getObject().refGetPath())) {
						i.remove();
						break;
					}
				}
			}
			for(Iterator<TraversedObject> i = nonCompositeStartingPoints.iterator(); i.hasNext();) {
				TraversedObject pi = i.next();
				for (Iterator<TraversedObject> j = allExportedObjects.iterator(); j.hasNext();) {
					TraversedObject pj = j.next();
					if(pi.getObject().refGetPath().size() > pj.getObject().refGetPath().size() && pi.getObject().refGetPath().startsWith(pj.getObject().refGetPath())) {
						i.remove();
						break;
					}
				}
			}
			nonCompositeStartingPoints.removeAll(roots);
			// Pass 2
			QuotaByteArrayOutputStream bs = new QuotaByteArrayOutputStream(Exporter.class.getName());
			out = isMultiFileExport ? new ZipOutputStream(bs) : bs;
			ps = new PrintStream(out, false, "UTF-8");
			exportParams.getContext().keySet().retainAll(Arrays.asList("template"));
			exportParams.resetEntryId();
			this.exportItem(
				out, 
				ps, 
				roots, 
				allExportedObjects = new ArrayList<TraversedObject>(),
				new ArrayList<TraversedObject>(), 
				exportParams
			);
			this.exportItem(
				out, 
				ps, 
				nonCompositeStartingPoints, 
				allExportedObjects, 
				new ArrayList<TraversedObject>(), 
				exportParams
			);
			// post-process context
			for (Object context : exportParams.getContext().values()) {
				if(context instanceof HSSFWorkbook) {
					try {
						((HSSFWorkbook) context).write(ps);
					}
					catch (IOException e) {
						throw new ServiceException(e);
					}
				}
			}
			ps.close();
			String contentMimeType = isMultiFileExport ? Base.MIME_TYPE_ZIP : exportParams.getMimeType();
			String contentName = "Export" + (contentMimeType.equals(Base.MIME_TYPE_ZIP) ? FILE_EXT_ZIP : contentMimeType.equals(MIME_TYPE_EXCEL) ? FILE_EXT_XLS : FILE_EXT_BIN);
			return new Object[] {
			    contentName, 
			    contentMimeType, 
			    bs.toByteArray()
			};
		}
		catch (IOException e) {
			throw new ServiceException(e);
		}
	}

	// -------------------------------------------------------------------------
	// Members
	// -------------------------------------------------------------------------
	public static final String MIME_TYPE_XML = "application/xml";

	public static final String MIME_TYPE_EXCEL = "application/x-excel";

	public static final String FILE_EXT_XLS = ".xls";

	public static final String FILE_EXT_XML = ".xml";

	public static final String FILE_EXT_BIN = ".bin";

	public static final String FILE_EXT_ZIP = ".zip";

	protected static final int MAX_LEVELS = 5;

	// ---------------------------------------------------------------------
	interface ContentHandler {

		public boolean startReference(String name) throws ServiceException;

		public void endReference(String reference) throws ServiceException;

		public boolean startObject(TraversedObject object, String id, String qualifierName, String qualifiedName) throws ServiceException;

		public void endObject(TraversedObject object, String qualifiedName) throws ServiceException;

		public boolean featureComplete(TraversedObject object, String referenceName) throws ServiceException;

		public void startTraversal(TraversedObject object) throws ServiceException;

		public void endTraversal(TraversedObject object) throws ServiceException;

	}

	// ---------------------------------------------------------------------
	protected Collection<?> getContent(
		TraversedObject object,
		String referenceName
	) {
		return ((RefContainer<?>) object.getObject().refGetValue(referenceName)).refGetAll(null);
	}
	
	// -------------------------------------------------------------------------
	class ObjectExporter {

		// ---------------------------------------------------------------------
		public ObjectExporter(
			PrintStream out, 
			ExportParams params,
			List<TraversedObject> exportedObjects
		) {
			this.out = out;
			this.params = params;
			this.referencedObjects = new ArrayList<TraversedObject>();
			this.exportedObjects = exportedObjects;
		}

		// ---------------------------------------------------------------------
		public List<TraversedObject> getReferencedObjects(
		) {
			return this.referencedObjects;
		}

		// ---------------------------------------------------------------------
		public List<TraversedObject> getExportedObjects(
		) {
			return this.exportedObjects;
		}

		// ---------------------------------------------------------------------
		public String getQualifierName(
			RefObject_1_0 object
		) throws ServiceException {
			Model_1_0 model = Model_1Factory.getModel();
			ModelElement_1_0 objectClass = model.getElement(object.refClass().refMofId());
			String qualifierName = null;
			if(!objectClass.objGetList("compositeReference").isEmpty()) {
				ModelElement_1_0 compReference = model.getElement(((Path) objectClass.objGetValue("compositeReference")).getBase());
				ModelElement_1_0 associationEnd = model.getElement(compReference.getReferencedEnd().getBase());
				qualifierName = (String) associationEnd.objGetValue("qualifierName");
			}
			else if("org:openmdx:base:Authority".equals(objectClass.getQualifiedName())) {
				qualifierName = "name";
			}
			else {
				throw new ServiceException(BasicException.Code.DEFAULT_DOMAIN, BasicException.Code.ASSERTION_FAILURE, "no composite reference found for class.", new BasicException.Parameter[] {
					new BasicException.Parameter("class", objectClass)
				});
			}
			return qualifierName;
		}

		// ---------------------------------------------------------------------
		public void export(
			TraversedObject object, 
			String referenceName, 
			String schemaString
		) throws ServiceException {
			ContentHandler contentHandler = null;
			if(Exporter.MIME_TYPE_XML.equals(this.params.getMimeType())) {
				contentHandler = new XMLContentHandler("http://www.w3.org/2001/XMLSchema-instance", schemaString);
			}
			else if(Exporter.MIME_TYPE_EXCEL.equals(this.params.getMimeType())) {
				contentHandler = new ExcelContentHandler(this.params.getContext());
			}
			else {
				throw new ServiceException(BasicException.Code.DEFAULT_DOMAIN, BasicException.Code.ASSERTION_FAILURE, "Unsupported mime type. Unable to export item.", new BasicException.Parameter(
				    "mimetype", this.params.getMimeType()));
			}
			contentHandler.startTraversal(object);
			this.exportObject(
				object, 
				referenceName, 
				contentHandler
			);
			contentHandler.endTraversal(object);
		}
		
		// ---------------------------------------------------------------------
		private void exportObject(
			TraversedObject object, 
			String reference, 
			ContentHandler contentHandler
		) throws ServiceException {
			if(!this.exportedObjects.contains(object)) {
				Model_1_0 model = Model_1Factory.getModel();
				String objectType = object.getObject().refClass().refMofId();
				String qualifierName = this.getQualifierName(object.getObject());
				contentHandler.startObject(
					object, 
					object.getObject().refGetPath().getBase(), 
					qualifierName, 
					objectType
				);
				contentHandler.featureComplete(
					object, 
					reference
				);
				@SuppressWarnings("unchecked")
                Map<String, ModelElement_1_0> references = model.getElement(objectType).objGetMap("reference");
				for (ModelElement_1_0 featureDef : references.values()) {
					ModelElement_1_0 referencedEnd = model.getElement(featureDef.getReferencedEnd());
					boolean referenceIsComposite = model.isReferenceType(featureDef) && AggregationKind.COMPOSITE.equals(referencedEnd.getAggregation());
					boolean referenceIsShared = model.isReferenceType(featureDef) && AggregationKind.SHARED.equals(referencedEnd.getAggregation());
					// Only navigate changeable references which are either
					// 'composite' or 'shared'
					// Do not navigate references with aggregation 'none'.
					if(referenceIsComposite || referenceIsShared) {
						String referenceName = (String) featureDef.getName();
						Set<String> referenceFilter = this.params.getReferenceFilter();
						boolean matches = referenceFilter == null;
						if(!matches) {
							String qualifiedReferenceName = (String) featureDef.getQualifiedName();
							for (int i = object.getLevel(); i < MAX_LEVELS; i++) {
								matches = 
									referenceFilter.contains(referenceName + "[" + i + "]") || 
									referenceFilter.contains(qualifiedReferenceName + "[" + i + "]");
								if(matches) break;
							}
						}
						if(matches) {
							Collection<?> content = Exporter.this.getContent(
								object, 
								referenceName
							);
							contentHandler.startReference(referenceName);
							for (Object contained : content) {
								if(contained instanceof RefObject_1_0) {
									this.exportObject(
										new TraversedObject(
											(RefObject_1_0) contained, 
											object.getLevel() // composites are at the same level as parents
									    ), 
									    referenceName, 
									    contentHandler
									);
								}
							}
							contentHandler.endReference(referenceName);
						}
					}
				}
				contentHandler.endObject(object, objectType);
				this.exportedObjects.add(object);
			}
		}

		// ---------------------------------------------------------------------
		// ExcelTraversalHandler
		// ---------------------------------------------------------------------
		class ExcelContentHandler implements ContentHandler {

			public ExcelContentHandler(
				Map<String, Object> context
			) {
				this.model = Model_1Factory.getModel();
				this.wb = (HSSFWorkbook) context.get("wb");
				if(this.wb == null) {
					HSSFWorkbook wb = null;
					// Create workbook from template
					if(context.get("template") != null) {
						Object template = context.get("template");
						try {
							if(template instanceof InputStream) {
								wb = new HSSFWorkbook((InputStream) template, true);
							}
							else if(template instanceof org.opencrx.kernel.document1.jmi1.Document) {
								org.opencrx.kernel.document1.jmi1.Document templateDocument = (org.opencrx.kernel.document1.jmi1.Document) template;
								DocumentRevision revision = templateDocument.getHeadRevision();
								if(revision instanceof MediaContent) {
									BinaryLargeObject content = ((MediaContent) revision).getContent();
									wb = new HSSFWorkbook(content.getContent(), true);
									// Replace template document by its stream
									// representation
									QuotaByteArrayOutputStream bos = new QuotaByteArrayOutputStream(Exporter.class.getName());
									wb.write(bos);
									bos.close();
									context.put("template", bos.toInputStream());
								}
							}
						}
						catch (Exception e) {
							ServiceException e0 = new ServiceException(e);
							e0.log();
						}
					}
					if(wb == null) {
						wb = new HSSFWorkbook();
					}
					context.put("wb", this.wb = wb);
				}
				this.cellStyleHidden = this.wb.createCellStyle();
				this.cellStyleHidden.setHidden(false);
				this.objectCount = 0;
				this.metainf = this.wb.getSheet("META-INF");
				HSSFCell mCell = (this.metainf == null) || (this.metainf.getRow(1) == null) ? null : this.metainf.getRow(1).getCell(this.getColumnIndex(this.metainf, "META-INF", "MaxObjects"));
				this.objectLimit = mCell == null ? MAX_OBJECTS : new Double(mCell.getNumericCellValue()).intValue();
			}

			private int getColumnIndex(
				HSSFSheet sheet, 
				String sheetName, 
				String attributeName
			) {
				HSSFRow heading = sheet.getRow(0);
				int num = 0;
				for (Iterator<Cell> i = heading.cellIterator(); i.hasNext();) {
					Cell cell = i.next();
					if(attributeName.equals(cell.getStringCellValue())) {
						return cell.getColumnIndex();
					}
					num++;
				}
				HSSFCell cell = heading.createCell(num);
				cell.setCellValue(new HSSFRichTextString(attributeName));
				// Set column width to 0 if attribute is not listed in export
				// options
				if(ObjectExporter.this.params.getOptions().get(sheetName) != null) {
					List<String> attributeNames = ObjectExporter.this.params.getOptions().get(sheetName);
					if(!attributeNames.isEmpty() && !attributeNames.contains(attributeName)) {
						sheet.setColumnWidth(num, (short) 0);
					}
				}
				return num;
			}

			@SuppressWarnings("unchecked")
			private Map<String, ModelElement_1_0> getAttributes(
				TraversedObject object
			) throws ServiceException {
				Map<String, ModelElement_1_0> attributes = this.model.getElement(
					object.getObject().refClass().refMofId()
				).objGetMap("attribute");
				return attributes;
			}

			private int getMaxValueSize(
				TraversedObject object
			) throws ServiceException {
				int maxIndex = 0;
				for (String attributeName : this.getAttributes(object).keySet()) {
					if(!NOT_EXPORTED_ATTRIBUTES.contains(attributeName)) {
						Object value = null;
						try {
							value = object.getObject().refGetValue(attributeName);
						}
						catch (Exception e) {
						}
						if(value instanceof Collection<?>) {
							maxIndex = Math.max(maxIndex, ((Collection<?>) value).size());
						}
						else if(value != null) {
							maxIndex = Math.max(maxIndex, 1);
						}
					}
				}
				return maxIndex;
			}

			public void endObject(
				TraversedObject object, 
				String qualifiedName
			) throws ServiceException {
			}

			public void endReference(
				String reference
			) throws ServiceException {
				this.sheet = null;
			}

			public void endTraversal(
				TraversedObject object
			) throws ServiceException {
			}

			public boolean featureComplete(
				TraversedObject object, 
				String referenceName
			) throws ServiceException {
				this.objectCount++;
				if(object.getObject().refGetPath().size() > 5) {
					String sheetName = referenceName;
					if(sheetName.length() > 0) {
						sheetName = Character.toUpperCase(sheetName.charAt(0)) + sheetName.substring(1);
					}
					this.sheet = this.wb.getSheet(sheetName);
					if(this.sheet == null) {
						this.sheet = this.wb.createSheet(sheetName);
						// Create named groups for DATA
						String cellName = sheetName + ".DATA";
						try {
							HSSFName namedCell = this.wb.createName();
							namedCell.setNameName(cellName);
							namedCell.setRefersToFormula(sheetName + "!$A$2:$CY$65536");
						}
						catch (Exception e) {
						}
						// Create named groups for COLUMN
						try {
							HSSFName namedCell = this.wb.createName();
							namedCell.setNameName(sheetName + ".COLUMN");
							namedCell.setRefersToFormula(sheetName + "!$A$1:$CY$1");
						}
						catch (Exception e) {
						}
						// Heading row which contains the attribute names
						this.sheet.createRow(0);
						// Create columns specified by export options
						if(ObjectExporter.this.params.getOptions().get(sheetName) != null) {
							List<String> attributeNames = ObjectExporter.this.params.getOptions().get(sheetName);
							for (String attributeName : attributeNames) {
								this.getColumnIndex(this.sheet, sheetName, attributeName);
							}
						}
					}
					int maxIndex = this.getMaxValueSize(object);
					String xri = object.getObject().refGetPath().toXRI();
					HSSFRichTextString parentXri = new HSSFRichTextString(object.getObject().refGetPath().getParent().getParent().toXRI());
					HSSFRichTextString objectId = new HSSFRichTextString(object.getObject().refGetPath().getBase());
					HSSFRichTextString objectClass = new HSSFRichTextString(object.getObject().refClass().refMofId());
					for (int i = 0; i < maxIndex; i++) {
						HSSFRow row = this.sheet.createRow(this.sheet.getLastRowNum() + 1);
						HSSFCell cell = row.createCell(this.getColumnIndex(this.sheet, sheetName, "XRI"));
						cell.setCellValue(new HSSFRichTextString(xri + (i > 0 ? "*" + i : "")));
						cell = row.createCell(this.getColumnIndex(this.sheet, sheetName, "XRI.PARENT"));
						cell.setCellValue(parentXri);
						cell = row.createCell(this.getColumnIndex(this.sheet, sheetName, "ID"));
						cell.setCellValue(objectId);
						cell = row.createCell(this.getColumnIndex(this.sheet, sheetName, "IDX"));
						cell.setCellValue(i);
						cell = row.createCell(this.getColumnIndex(this.sheet, sheetName, SystemAttributes.OBJECT_CLASS));
						cell.setCellValue(objectClass);
						Map<String, ModelElement_1_0> attributes = this.getAttributes(object);
						for (String attributeName : attributes.keySet()) {
							Object v = null;
							try {
								v = object.getObject().refGetValue(attributeName);
							}
							catch (Exception e) {
							}
							List<Object> values = new ArrayList<Object>();
							if(v instanceof Collection<?>) {
								try {
									values.addAll((Collection<?>) v);
								}
								// Ignore in case some values can not be
								// retrieved
								catch (Exception e) {
								}
							}
							else if(v != null) {
								values.add(v);
							}
							Object value = i < values.size() ? values.get(i) : null;
							if(!NOT_EXPORTED_ATTRIBUTES.contains(attributeName)) {
								if(value != null) {
									cell = row.createCell(this.getColumnIndex(this.sheet, sheetName, attributeName));
									if(value instanceof InputStream) {
										try {
											QuotaByteArrayOutputStream bytes = new QuotaByteArrayOutputStream(Exporter.class.getName());
											BinaryLargeObjects.streamCopy((InputStream)value, 0, bytes);
											String encodedBytes = Base64.encode(bytes.getBuffer(), 0, bytes.size());
											if(encodedBytes.length() < Short.MAX_VALUE) {
												cell.setCellValue(new HSSFRichTextString(encodedBytes));
											}
										} catch(Exception e) {
											throw new ServiceException(e);
										}
									}
									else if(value instanceof byte[]) {
										String encodedBytes = Base64.encode((byte[]) value);
										if(encodedBytes.length() < Short.MAX_VALUE) {
											cell.setCellValue(new HSSFRichTextString(encodedBytes));
										}
									}
									else if(value instanceof Number) {
										cell.setCellValue(((Number) value).doubleValue());
									}
									else if(value instanceof Boolean) {
										cell.setCellValue(((Boolean) value).booleanValue());
									}
									else if(value instanceof RefObject_1_0) {
										cell.setCellValue(new HSSFRichTextString(((RefObject_1_0) value).refGetPath().toXRI()));
										Set<String> referenceFilter = ObjectExporter.this.params.getReferenceFilter();
										boolean matches = referenceFilter == null;
										if(!matches) {
											ModelElement_1_0 featureDef = attributes.get(attributeName);
											if(featureDef != null) {
												String qualifiedFeatureName = (String) featureDef.getQualifiedName();
												matches = referenceFilter.contains(attributeName + "[1]") || referenceFilter.contains(qualifiedFeatureName + "[1]");
											}
										}
										if(matches) {
											TraversedObject obj = new TraversedObject(
												(RefObject_1_0) value, 
												object.getLevel() + 1 // increase level for referenced objects
										    );
											if(!ObjectExporter.this.referencedObjects.contains(obj)) {
												ObjectExporter.this.referencedObjects.add(obj);
											}
										}
									}
									else {
										String stringifiedValue = value.toString();
										if(stringifiedValue.length() > Short.MAX_VALUE) {
											cell.setCellValue(new HSSFRichTextString(stringifiedValue.substring(0, Short.MAX_VALUE - 5) + "..."));
										}
										else {
											cell.setCellValue(new HSSFRichTextString(stringifiedValue));
										}
									}
								}
							}
						}
					}
					HSSFCell mCell = (this.metainf == null) || (this.metainf.getRow(1) == null) ? null : this.metainf.getRow(1).getCell(this.getColumnIndex(this.metainf, "META-INF", sheetName));
					int rowLimit = mCell == null ? this.objectLimit : new Double(mCell.getNumericCellValue()).intValue();
					return this.sheet.getLastRowNum() < rowLimit;
				}
				else {
					return this.objectCount < this.objectLimit;
				}
			}

			public boolean startObject(
				TraversedObject object, 
				String id, 
				String qualifierName, 
				String qualifiedName
			) throws ServiceException {
				return this.objectCount < this.objectLimit;
			}

			public boolean startReference(String qualifiedName) throws ServiceException {
				return true;
			}

			public void startTraversal(TraversedObject object) throws ServiceException {
				this.sheet = null;
			}

			private final static int MAX_OBJECTS = 2000;

			private final Model_1_0 model;

			private HSSFWorkbook wb = null;

			private HSSFSheet sheet = null;

			private HSSFSheet metainf = null;

			private HSSFCellStyle cellStyleHidden = null;

			private int objectLimit;

			private int objectCount;

		}

		// ---------------------------------------------------------------------
		// XMLContentHandler
		// ---------------------------------------------------------------------
		class XMLContentHandler implements ContentHandler {

			public XMLContentHandler(String schemaInstance, String schemaLocation) {
				this.model = Model_1Factory.getModel();
				this.schemaInstance = schemaInstance;
				this.schemaLocation = schemaLocation;
				this.xmlWriter = new XMLWriter(ObjectExporter.this.out);
			}

			private String toXML(Object elementName) {
				return ((String) elementName).replace(':', '.');
			}

			private String toSimpleQualifiedName(String qualifiedName) {
				return qualifiedName.substring(qualifiedName.lastIndexOf(':') + 1);
			}

			public void endObject(TraversedObject object, String qualifiedName) throws ServiceException {
				this.xmlWriter.endElement("", "", "_content", true);
				String endElem = this.toXML(qualifiedName);
				this.xmlWriter.endElement("", "", endElem, true);
			}

			public void endReference(String reference) throws ServiceException {
				this.xmlWriter.endElement("", "", this.toSimpleQualifiedName(reference), true);
			}

			public boolean featureComplete(TraversedObject object, String referenceName) throws ServiceException {
				Map<String, String> tags = null;
				ModelElement_1_0 objectClass = this.model.getElement(object.getObject().refClass().refMofId());
				this.xmlWriter.startElement("", "", "_object", new XMLWriter.Attributes());
				Map<String, ModelElement_1_0> modelAttributes = this.model.getAttributeDefs(objectClass, true, true);
				for (String attributeName : modelAttributes.keySet()) {
					if((objectClass != null) && !NOT_EXPORTED_ATTRIBUTES.contains(attributeName)) {
						ModelElement_1_0 attributeDef = modelAttributes.get(attributeName);
						Object attributeValue = null;
						try {
							attributeValue = object.getObject().refGetValue(attributeName);
						}
						catch (Exception e) {
						}
						if((attributeDef == null) || (attributeValue == null)) {
							continue;
						}
						Multiplicity multiplicity = ModelHelper.getMultiplicity(attributeDef);
						boolean isMultiValued = 
							multiplicity == Multiplicity.SET || 
							multiplicity == Multiplicity.LIST || 
							multiplicity == Multiplicity.SPARSEARRAY ||
							multiplicity == Multiplicity.MAP;
						boolean needsPosition = multiplicity == Multiplicity.SPARSEARRAY;
						String elementTag = this.toSimpleQualifiedName((String) attributeDef.getQualifiedName());
						List<Object> attributeValues = new ArrayList<Object>();
						if(attributeValue instanceof Collection<?>) {
							try {
								attributeValues.addAll((Collection<?>) attributeValue);
							}
							// In case attribute value is an object which is not
							// accessible ignore it
							catch (Exception e) {
							}
						}
						else {
							attributeValues.add(attributeValue);
						}
						if(!attributeValues.isEmpty()) {
							this.xmlWriter.startElement("", "", elementTag, new XMLWriter.Attributes());
							int valueIndex = 0;
							for (Object value : attributeValues) {
								String stringValue = null;
								ModelElement_1_0 attributeType = this.model.getDereferencedType(attributeDef.getType());
								String typeName = (String) attributeType.getQualifiedName();
								if(PrimitiveTypes.DATETIME.equals(typeName)) {
									String v = DateTimeFormat.BASIC_UTC_FORMAT.format((Date) value);
									String t = v.substring(0, 4) + "-" + v.substring(4, 6) + "-" + v.substring(6, 11) + ":" + v.substring(11, 13) + ":" + v.substring(13, 20);
									stringValue = t;
								}
								else if(PrimitiveTypes.DATE.equals(typeName)) {
									XMLGregorianCalendar v = (XMLGregorianCalendar) value;
									String t = v.getYear() + "-" + (v.getMonth() < 10 ? "0" + v.getMonth() : v.getMonth()) + "-" + (v.getDay() < 10 ? "0" + v.getDay() : v.getDay());
									stringValue = t;
								}
								else if(PrimitiveTypes.LONG.equals(typeName) || PrimitiveTypes.INTEGER.equals(typeName) || PrimitiveTypes.SHORT.equals(typeName)) {
									value = new Long(((Number) value).longValue());
									stringValue = value.toString();
								}
								else if(PrimitiveTypes.BINARY.equals(typeName)) {
									if(value instanceof byte[]) {
										stringValue = Base64.encode((byte[]) value);
									}
									else {
										stringValue = value.toString();
									}
								}
								else if(value instanceof RefObject_1_0) {
									RefObject_1_0 obj = (RefObject_1_0) value;
									stringValue = obj.refGetPath().toXRI();
									TraversedObject o = new TraversedObject(
										obj, 
										object.getLevel() + 1 // increase level for referenced objects
									);
									if(!ObjectExporter.this.referencedObjects.contains(o)) {
										ObjectExporter.this.referencedObjects.add(o);
									}
								}
								else {
									stringValue = value.toString();
								}
								XMLWriter.Attributes atts = new XMLWriter.Attributes();
								if(needsPosition) {
									atts.addCDATA("_position", String.valueOf(valueIndex));
								}
								if(isMultiValued) {
									this.xmlWriter.startElement("", "", "_item", atts);
								}
								this.xmlWriter.characters(stringValue.toCharArray(), 0, stringValue.length());
								if(isMultiValued) {
									this.xmlWriter.endElement("", "", "_item", false);
								}
								valueIndex++;
							}
							this.xmlWriter.endElement("", "", elementTag, isMultiValued);
							// generate attribute tag as comment
							if((tags != null) && (tags.get(attributeName) != null)) {
								this.xmlWriter.comment(tags.get(attributeName));
							}
						}
					}
				}
				this.xmlWriter.endElement("", "", "_object", true);
				this.xmlWriter.startElement("", "", "_content", new XMLWriter.Attributes());
				return true;
			}

			public boolean startObject(
				TraversedObject object, 
				String id, 
				String qualifierName, 
				String qualifiedName
			) throws ServiceException {
				XMLWriter.Attributes atts = new XMLWriter.Attributes();
				atts.addCDATA(qualifierName, object.getObject().refGetPath().getBase());
				this.xmlWriter.startElement("", "", this.toXML(qualifiedName), atts);
				return true;
			}

			public boolean startReference(String name) throws ServiceException {
				this.xmlWriter.startElement("", "", this.toSimpleQualifiedName(name), new XMLWriter.Attributes());

				return true;
			}

			public void endTraversal(TraversedObject object) throws ServiceException {
				PersistenceManager pm = JDOHelper.getPersistenceManager(object.getObject());
				for (int i = object.getObject().refGetPath().size() - 2; i > 0; i -= 2) {
					RefObject_1_0 parent = (RefObject_1_0) pm.getObjectById(object.getObject().refGetPath().getPrefix(i));
					String qualifiedTypeName = parent.refClass().refMofId();
					this.xmlWriter.endElement("", "", object.getObject().refGetPath().get(i), true);
					this.xmlWriter.endElement("", "", "_content", true);
					this.xmlWriter.endElement("", "", this.toXML(qualifiedTypeName), true);
				}
				this.xmlWriter.endDocument();
			}

			public void startTraversal(TraversedObject object) throws ServiceException {
				PersistenceManager pm = JDOHelper.getPersistenceManager(object.getObject());
				this.xmlWriter.startDocument();
				for (int i = 1; i < object.getObject().refGetPath().size(); i += 2) {
					RefObject_1_0 parent = (RefObject_1_0) pm.getObjectById(object.getObject().refGetPath().getPrefix(i));
					String qualifiedTypeName = parent.refClass().refMofId();
					String qualifierName = ObjectExporter.this.getQualifierName(parent);
					XMLWriter.Attributes atts = new XMLWriter.Attributes();
					atts.addCDATA(qualifierName, parent.refGetPath().getBase());
					atts.addCDATA("_operation", "null");
					if(qualifiedTypeName.equals("org:openmdx:base:Authority")) {
						atts.addCDATA("xmlns:xsi", this.schemaInstance);
						atts.addCDATA("xsi:noNamespaceSchemaLocation", this.schemaLocation);
					}
					this.xmlWriter.startElement("", "", this.toXML(qualifiedTypeName), atts);
					this.xmlWriter.startElement("", "", "_object", new XMLWriter.Attributes());
					this.xmlWriter.endElement("", "", "_object", true);
					this.xmlWriter.startElement("", "", "_content", new XMLWriter.Attributes());
					this.xmlWriter.startElement("", "", object.getObject().refGetPath().get(i), new XMLWriter.Attributes());
				}
			}

			final Model_1_0 model;

			final String schemaInstance;

			final String schemaLocation;

			private final XMLWriter xmlWriter;

		}

		// ---------------------------------------------------------------------
		final Set<String> NOT_EXPORTED_ATTRIBUTES = new HashSet<String>(Arrays.asList(SystemAttributes.OBJECT_INSTANCE_OF));

		final List<TraversedObject> referencedObjects;

		final List<TraversedObject> exportedObjects;

		final ExportParams params;

		final PrintStream out;
	}

}
