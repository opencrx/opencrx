/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: GridExportObjectsAction
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2011-2013, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.portal.action;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRichTextString;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.opencrx.kernel.backend.Exporter;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.io.QuotaByteArrayOutputStream;
import org.openmdx.base.naming.Path;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ViewsCache;
import org.openmdx.portal.servlet.action.ActionPerformResult;
import org.openmdx.portal.servlet.action.BoundAction;
import org.openmdx.portal.servlet.attribute.AttributeValue;
import org.openmdx.portal.servlet.attribute.ObjectReferenceValue;
import org.openmdx.portal.servlet.component.Grid;
import org.openmdx.portal.servlet.component.ObjectView;
import org.openmdx.portal.servlet.component.ReferencePane;
import org.openmdx.portal.servlet.component.ShowObjectView;
import org.openmdx.portal.servlet.component.UiGrid;

/**
 * GridExportObjectsAction
 *
 */
public abstract class ExportObjectsAction extends BoundAction {

	/**
	 * GridExporter
	 *
	 */
	public interface GridExporter {
		
		/**
		 * Export grid starting from given object.
		 * 
		 * @param startFrom
		 * @return
		 * @throws ServiceException
		 */
		Object[] exportItem(
			RefObject_1_0 startFrom
		) throws ServiceException ;
		
	}

	/**
	 * ModelBasedGridExporter. Grid exporter which exports all attributes of the
	 * selected grid objects according the model information.
	 *
	 */
	static class ModelBasedGridExporter extends Exporter implements GridExporter {

		/**
		 * Constructor.
		 * 
		 * @param grid
		 * @param selectedObjectIdentities
		 * @param mimeType
		 * @param referenceFilter
		 * @param maxItems
		 */
		public ModelBasedGridExporter(
			UiGrid grid,
			List<Path> selectedObjectIdentities,
			String mimeType,
			String referenceFilter,
			int maxItems
		) {
	        this.grid = grid;
	        this.selectedObjectIdentities = selectedObjectIdentities;
	        this.mimeType = mimeType;
	        this.referenceFilter = referenceFilter;
	        this.maxItems = maxItems;
        }
		
		/* (non-Javadoc)
		 * @see org.opencrx.kernel.portal.action.GridExportObjectsAction.GridExporter#exportItem(org.openmdx.base.accessor.jmi.cci.RefObject_1_0)
		 */
		@Override
        public Object[] exportItem(
        	RefObject_1_0 startFrom 
        ) throws ServiceException {
			return super.exportItem(
				startFrom, 
				null, // exportProfile
				this.referenceFilter, 
				this.mimeType
			);
        }
		
		/* (non-Javadoc)
		 * @see org.opencrx.kernel.backend.Exporter#getContent(org.opencrx.kernel.backend.Exporter.TraversedObject, java.lang.String)
		 */
		@Override
        protected Collection<?> getContent(
        	TraversedObject startingFrom, 
        	String referenceName
        ) {	
			PersistenceManager pm = JDOHelper.getPersistenceManager(startingFrom.getObject());
			List<Object> content = new ArrayList<Object>();
			// Export selected / displayed grid objects.
			if(
				this.grid.getReferenceName().equals(referenceName) &&
				this.maxItems < Integer.MAX_VALUE
			) {
				// Export selected objects only
				if(this.selectedObjectIdentities != null && !this.selectedObjectIdentities.isEmpty()) {
					for(Path identity: this.selectedObjectIdentities) {
						content.add(
							pm.getObjectById(identity)
						);
					}
				} else {
					// Export objects of current page
					List<UiGrid.GridRow> rows = this.grid.getRows(pm);
					for(UiGrid.GridRow row: rows) {
						List<Object> cells = row.getCells();
						if(cells != null && !cells.isEmpty()) {
							content.add(
								((ObjectReferenceValue)cells.get(0)).getObject()
							);
						}
					}
				}
			} else {
				Collection<?> objs = super.getContent(
		        	startingFrom, 
		        	referenceName
		        );
				int count = 0;
				for(Object obj: objs) {
					content.add(obj);
					count++;
					// Do not export more than maxItems objects
					if(count > this.maxItems) break;
				}
			}
			return content;
        }

		private final UiGrid grid;
		private final List<Path> selectedObjectIdentities;
		private final String mimeType;
		private final String referenceFilter;
		private final int maxItems;
	}
	
	/**
	 * WysiwygBasedGridExporter. Export which exports the UI customized grid columns.
	 *
	 */
	static class WysiwygBasedGridExporter implements GridExporter {

		/**
		 * Constructor.
		 * 
		 * @param grid
		 * @param selectedObjectIdentities
		 * @param mimeType
		 * @param referenceFilter
		 * @param maxItems
		 */
		public WysiwygBasedGridExporter(
			UiGrid grid,
			List<Path> selectedObjectIdentities,
			String mimeType,
			boolean allColumns,
			int maxItems
		) {
	        this.grid = grid;
	        this.selectedObjectIdentities = selectedObjectIdentities;
	        this.mimeType = mimeType;
	        this.allColumns = allColumns;
	        this.maxItems = maxItems;
        }

		/**
		 * Export grid row and append to sheet.
		 * 
		 * @param sheet
		 * @param rowNum
		 * @param cells
		 * @throws ServiceException
		 */
		protected void exportRow(
			HSSFSheet sheet,
			int rowNum,
			List<Object> cells
		) throws ServiceException {
			// Prepare heading
			if(rowNum == 0) {
				// Prepare heading
				{
					HSSFRow heading = sheet.createRow(0);
					// XRI
					{
						HSSFCell cell = heading.createCell(0);
						cell.setCellValue(new HSSFRichTextString("XRI"));					
					}
					List<Action> columnOrderActions = this.grid.getColumnOrderActions();
					for(int i = 1; i < cells.size(); i++) {
						Action columnOrderAction = columnOrderActions.get(i);
						HSSFCell cell = heading.createCell(i);
						cell.setCellValue(new HSSFRichTextString(columnOrderAction.getToolTip()));
					}
				}
			}
			HSSFRow row = sheet.createRow(rowNum + 1);
			// XRI
			{
				RefObject_1_0 object = (RefObject_1_0)((ObjectReferenceValue)cells.get(0)).getObject();
				HSSFCell cell = row.createCell(0);
				cell.setCellValue(new HSSFRichTextString(object.refGetPath().toXRI()));				
			}
			for(int i = 1; i < cells.size(); i++) {
				AttributeValue valueHolder = (AttributeValue)cells.get(i);
				HSSFCell cell = row.createCell(i);
				String stringifiedValue = valueHolder == null ? null : valueHolder.toString();
				stringifiedValue = stringifiedValue == null ? "" : stringifiedValue;
				// Remove brackets for collections
				if(stringifiedValue.startsWith("[") && stringifiedValue.endsWith("]")) {
					stringifiedValue = stringifiedValue.substring(1, stringifiedValue.length() - 1);
				}
				cell.setCellValue(
					new HSSFRichTextString(stringifiedValue)
				);
			}
		}

		/* (non-Javadoc)
		 * @see org.opencrx.kernel.portal.action.GridExportObjectsAction.GridExporter#exportItem(org.openmdx.base.accessor.jmi.cci.RefObject_1_0)
		 */
		@Override
        public Object[] exportItem(
        	RefObject_1_0 startFrom
        ) throws ServiceException {
			PersistenceManager pm = JDOHelper.getPersistenceManager(startFrom);
			HSSFWorkbook wb = new HSSFWorkbook();
			HSSFSheet sheet = wb.createSheet(this.grid.getToolTip().get(0));
			// Export (selected) objects of current grid page
			if(
				this.maxItems < Short.MAX_VALUE ||
				(this.selectedObjectIdentities != null && !this.selectedObjectIdentities.isEmpty())
			) {
				List<UiGrid.GridRow> rows = this.grid.getRows(
					pm, 
					this.allColumns
				);
				int rowNum = 0;
				for(UiGrid.GridRow row: rows) {
					List<Object> cells = row.getCells();
					if(cells != null && !cells.isEmpty()) {
						RefObject_1_0 object = (RefObject_1_0)((ObjectReferenceValue)cells.get(0)).getObject();
						if(
							this.selectedObjectIdentities == null ||
							this.selectedObjectIdentities.isEmpty() ||
							this.selectedObjectIdentities.contains(object.refGetPath())
						) {
							try {
								this.exportRow(
									sheet,
									rowNum,
									cells
								);
							} catch(Exception ignore) {}
							rowNum++;
						}
					}
				}
			} else {
				// Export objects starting from page 0 up to maxItems
				boolean showRows = this.grid.getShowRows();
				int pageSize = this.grid.getPageSize();
				this.grid.setShowRows(true);
				int currentPage = 0;
				int rowNum = 0;
				while(true) {
					this.grid.setPage(
						currentPage, 
						UiGrid.MAX_PAGE_SIZE
					);
					List<UiGrid.GridRow> rows = this.grid.getRows(
						pm, 
						this.allColumns
					);
					for(UiGrid.GridRow row: rows) {
						List<Object> cells = row.getCells();
						if(cells != null && !cells.isEmpty()) {
							try {
								this.exportRow(
									sheet, 
									rowNum, 
									cells
								);
								rowNum++;
							} catch(Exception ignore) {}
						}
					}
					// Done when exported more than maxItems rows or
					// page has less then page size rows					
					if(
						rows.size() < this.grid.getPageSize() || 
						rowNum > this.maxItems
					) {
						break;
					}
					currentPage++;
				}
				this.grid.setPage(
					0, 
					pageSize
				);
				this.grid.setShowRows(showRows);
			}
			QuotaByteArrayOutputStream bs = new QuotaByteArrayOutputStream(Exporter.class.getName());
			try {
				wb.write(bs);
				bs.close();
			} catch(Exception ignore) {}
			String contentMimeType = this.mimeType;
			String contentName = "Export" + Exporter.FILE_EXT_XLS;
			return new Object[] {
			    contentName, 
			    contentMimeType, 
			    bs.toByteArray()
			};
        }

		private final UiGrid grid;
		private final List<Path> selectedObjectIdentities;
		private final String mimeType;
		private final boolean allColumns;
		private final int maxItems;

	}

	/**
	 * Get grid exporter. Must be implemented by concrete sub-class.
	 * 
	 * @param grid
	 * @param selectedObjectIdentities
	 * @param maxItems
	 * @return
	 * @throws ServiceException
	 */
	protected abstract GridExporter getGridExporter(
		UiGrid grid,
		List<Path> selectedObjectIdentities,
		int maxItems
	) throws ServiceException;
	
	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.action.BoundAction#perform(org.openmdx.portal.servlet.view.ObjectView, javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse, java.lang.String, javax.servlet.http.HttpSession, java.util.Map, org.openmdx.portal.servlet.ViewsCache, org.openmdx.portal.servlet.ViewsCache)
	 */
	@Override
    public ActionPerformResult perform(
        ObjectView view,
        HttpServletRequest request,
        HttpServletResponse response,        
        String parameter,
        HttpSession session,
        Map<String,String[]> requestParameters,
        ViewsCache editViewsCache,
        ViewsCache showViewsCache      
    ) throws IOException, ServletException {
        ApplicationContext app = view.getApplicationContext();
        if(view instanceof ShowObjectView) {
            ShowObjectView currentView = (ShowObjectView)view;    	
        	PersistenceManager pm = app.getNewPmData();
	    	try {
	            int paneIndex = -1;
	            try { 
	            	paneIndex = Integer.parseInt(requestParameters.get(Action.PARAMETER_PANE)[0]);
	            } catch(Exception e) {}
	            int referenceIndex = -1;
	            try {
	            	referenceIndex = Integer.parseInt(requestParameters.get(Action.PARAMETER_REFERENCE)[0]);
	            } catch(Exception e) {}
	            List<ReferencePane> referencePanes = currentView.getChildren(ReferencePane.class);
	            if(paneIndex < referencePanes.size()) {
	                currentView.selectReferencePane(paneIndex);
	                referencePanes.get(paneIndex).selectReference(referenceIndex);
	                Grid grid = referencePanes.get(paneIndex).getGrid();
	                if(grid instanceof UiGrid) {
	                	UiGrid uiGrid = (UiGrid)grid;
	                	List<Path> selectedObjectIdentities = new ArrayList<Path>();
	                    StringTokenizer tokenizer = new StringTokenizer(parameter, " ");
	                    while(tokenizer.hasMoreTokens()) {
	                    	try {
		                        selectedObjectIdentities.add(
		                        	new Path(Action.getParameter(tokenizer.nextToken(), Action.PARAMETER_OBJECTXRI))
		                        );
	                    	} catch(Exception e) {}
	                    }
	                    int maxItems = 500; // default maxItems
	                    try {
	                    	maxItems = Integer.parseInt(requestParameters.get(Action.PARAMETER_SIZE)[0]);
	                    } catch(Exception e) {}
			    		GridExporter exporter = this.getGridExporter(
			    			uiGrid,
			    			selectedObjectIdentities,
			    			maxItems
			    		);
			    		String referenceName = uiGrid.getReferenceName();
			    		Object[] item = exporter.exportItem(
			    			view.getObject()
			    		);
			    		if(item != null) {
					        response.setContentType((String)item[1]);
					        response.setHeader("Content-disposition", "attachment;filename=" + referenceName + "-" + item[0]);            
					        OutputStream os = response.getOutputStream();
					        byte[] bytes = (byte[])item[2];
					        for(int i = 0; i < bytes.length; i++) {
					            os.write(bytes[i]);
					        }
					        response.setContentLength(bytes.length);
					        os.close();
			    		}
	                }
	            }
	    	} catch (Exception e) {
	            ServiceException e0 = new ServiceException(e);
	            SysLog.warning(e0.getMessage(), e0.getCause());
	            try {
	                pm.currentTransaction().rollback();
	            } catch(Exception e1) {}
	        }	        
	        pm.close();
        }
        return new ActionPerformResult(
        	ActionPerformResult.StatusCode.DONE
        );
    }

	
}
