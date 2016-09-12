/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.openmdx.org/
 * Description: Export activities and resources to MSProject 2003 xml format
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2006, CRIXP Corp., Switzerland
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
package org.opencrx.application.msproject;

import java.io.BufferedWriter;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.activity1.cci2.ActivityWorkRecordQuery;
import org.opencrx.kernel.activity1.cci2.ResourceAssignmentQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.Activity1Package;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityLinkTo;
import org.opencrx.kernel.activity1.jmi1.ActivityWorkRecord;
import org.opencrx.kernel.activity1.jmi1.Resource;
import org.opencrx.kernel.activity1.jmi1.ResourceAssignment;
import org.opencrx.kernel.backend.Activities.WorkRecordType;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.text.conversion.XMLEncoder;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.action.SelectObjectAction;


public class ProjectExporter {
  
    //-----------------------------------------------------------------------
    private static class ActivityMapper {
      
        public ActivityMapper(
            Activity a
        ) {
          this.activity = a;
        }
        Activity getActivity() {
          return this.activity;
        }
        int getUid() {
          return this.uid;
        }
        void setUid(int uid){
          this.uid = uid;
        }
        boolean isRootActivity() {
          return this.isRoot;
        }
        Collection<ActivityMapper> getPartActivityMappers() {
          return this.partActivityMappers.values();
        }
        Collection<ActivityMapper> getPredecessorActivityMappers() {
          return this.predecessorActivityMappers.values();
        }
        void addPartActivityMapper(ActivityMapper an) {
          this.partActivityMappers.put(an.activity.getIdentity(),an);
          an.isRoot = false;
        }
        void addPredecessorActivityMapper(ActivityMapper an) {
          this.predecessorActivityMappers.put(an.activity.getIdentity(),an);
        }
        void setTaskAttributes(
            Date start, 
            Date finish, 
            Date actualFinish 
        ) {
          this.taskStart = start;
          this.taskFinish = finish;
        }
        Date getTaskStart() {
          return this.taskStart;
        }
        Date getTaskFinish() {
          return this.taskFinish;
        }

        //-----------------------------------------------------------------------
        // Members
        //-----------------------------------------------------------------------    
        private Activity activity;
        private int uid;
        private boolean isRoot = true; //is not part of any other activity
        private Map<String,ActivityMapper> partActivityMappers = new HashMap<String,ActivityMapper>(); //Use a map to be sure to have no duplicates
        private Map<String,ActivityMapper> predecessorActivityMappers = new HashMap<String,ActivityMapper>();//Use a map to be sure to have no duplicates
        private Date taskStart = null;
        private Date taskFinish = null;
    }
  
    //-----------------------------------------------------------------------
    private static class ResourceMapper {
        
        public ResourceMapper(
            Resource r, 
            int uid
        ) {
          this.resource = r;
          this.uid = uid;
        }
        Resource getResource() {
          return this.resource;
        }
        int getUid() {
          return this.uid;
        }
        //-----------------------------------------------------------------------
        // Members
        //-----------------------------------------------------------------------    
        private Resource resource;
        private int uid;
    }
  
    //-----------------------------------------------------------------------
    private static class AssignmentMapper {
        
        public AssignmentMapper(
            int uid, 
            ResourceAssignment ra, 
            ActivityMapper am, 
            ResourceMapper rm
        ) {
          PersistenceManager pm = JDOHelper.getPersistenceManager(ra);
          this.resourceAssignment = ra;
          this.activityMapper = am;
          this.resourceMapper = rm;
          this.uid = uid;
          ActivityWorkRecordQuery workRecordQuery = (ActivityWorkRecordQuery)pm.newQuery(ActivityWorkRecord.class);
          workRecordQuery.recordType().elementOf(
        	  WorkRecordType.OVERTIME.getValue(), 
        	  WorkRecordType.STANDARD.getValue()
          );
          List<ActivityWorkRecord> workRecords = ra.getWorkRecord(workRecordQuery);
          BigDecimal totalAmount = new BigDecimal(0.0);
          for(ActivityWorkRecord workRecord: workRecords) {
              totalAmount = totalAmount.add(workRecord.getQuantity());
          }
          this.actualWorkHours = totalAmount.intValue();          
          this.workDurationPercentage = ra.getWorkDurationPercentage() == null ? 
        	  (short)100 : 
        	  ra.getWorkDurationPercentage().shortValue();
        }
        
        public int getUid() {
          return uid;
        }
        
        public int getTaskUid() {
          return this.activityMapper.getUid();
        }
        
        public int getResourceUid() {
          return this.resourceMapper.getUid();
        }
        
        public Date getFinish() {
          return this.activityMapper.getTaskFinish();
        }
        
        public Date getStart() {
          return this.activityMapper.getTaskStart();
        }
        
        public ResourceAssignment getResourceAssignment() {
          return this.resourceAssignment;
        }
        
        public short getWorkDurationPercentage(
        ) {
            return this.workDurationPercentage;
        }
        
        public int getActualWorkHours(
        ) {
            return this.actualWorkHours;
        }
        
        //-----------------------------------------------------------------------
        // Members
        //-----------------------------------------------------------------------    
        private ResourceAssignment resourceAssignment;
        private ActivityMapper activityMapper;
        private ResourceMapper resourceMapper;
        private int uid;
        private short workDurationPercentage;
        private int actualWorkHours;
    }
  
    //-----------------------------------------------------------------------
    public ProjectExporter(
        OutputStream os, 
        ActivityGroup activityGroup, 
        PersistenceManager pm
    ) {
        this.setOutputStream(os);
        this.setActivityGroup(activityGroup);
        this.pm = pm;
    }

    //-------------------------------------------------------------------------
    private String toFormatedStartDate(
        Date startDate
    ) {
        GregorianCalendar start = new GregorianCalendar();
        start.setTime(startDate);
        return this.startDateFormater.format(start.getTime());
    }
  
    //-------------------------------------------------------------------------
    String toFormatedEndDate(
        Date endDate
    ) {
        GregorianCalendar end = new GregorianCalendar();
        end.setTime(endDate);
        return this.endDateFormater.format(end.getTime());
    }
      
  //-----------------------------------------------------------------------
  public void setMspHyperlinkRootAddress(String mspHyperlinkRootAddress) {
    this.mspHyperlinkRootAddress = mspHyperlinkRootAddress;
  }
  
  //-----------------------------------------------------------------------
  public void setMspTaskType(short mspTaskType) {
    this.mspTaskType = mspTaskType;
  }
  
  //-----------------------------------------------------------------------
  public void setActivityGroup(ActivityGroup activityGroup) {
    this.activityGroup = activityGroup;
  }

  //-----------------------------------------------------------------------
  public void setExportActivities(Boolean enable) {
    exportActivities = (enable == null ? true : enable.booleanValue());
  }

  //-----------------------------------------------------------------------
  public void setExportResources(Boolean enable) {
    exportResources = (enable == null ? true : enable.booleanValue());
  }
  //-----------------------------------------------------------------------
  public void setExportAssignments(Boolean enable) {
    exportAssignments = (enable == null ? true : enable.booleanValue());
  }

  //-----------------------------------------------------------------------
  public void setActivityLinkTypeCodeIsParentOf(short isParentOf) {
    this.isParentOf = isParentOf;
  }

  //-----------------------------------------------------------------------
  public void setActivityLinkTypeCodeIsChildOf(short isChildOf) {
    this.isChildOf = isChildOf;
  }
  
  //-----------------------------------------------------------------------
  public void setActivityLinkTypeCodeIsPartOf(short isPartOf) {
    this.isPartOf = isPartOf;
  }
  
  //-----------------------------------------------------------------------
  public void setActivityLinkTypeCodeIsContainerOf(short isContainerOf) {
    this.isContainerOf = isContainerOf;
  }
  
  //-----------------------------------------------------------------------
  public void setOutputStream(OutputStream export) {
    try {
      this.pw = new PrintWriter(new BufferedWriter(new OutputStreamWriter(export, "UTF-8")));
    }
    catch (UnsupportedEncodingException e) {
      //should never happen
      SysLog.error("Failed to open writer using UTF-8 charset, use default charset of JVM", e);
      this.pw = new PrintWriter(new BufferedWriter(new OutputStreamWriter(export)));
    }
  }
  
  //-----------------------------------------------------------------------
  /**
   * Mapping
   * <table>
   * <tr><th>openCRX</th><th>MSProject</th></tr>
   * <tr><td>ActivityTracker.name</td><td>Project.Title</td></tr>
   * <tr><td>ActivityTracker.identity</td><td>Project.Subject</td></tr>
   * <tr><td>ActivityTracker.createdBy</td><td>Project.Author</td></tr>
   * <tr><td>ActivityTracker.category</td><td>Project.Category</td></tr>
   * <tr><td>ActivityTracker.description</td><td>Project.task(uid=0).Notes</td></tr>
   * <tr><td>ActivityTracker.actualEffortHours and acutalEffortMinutes</td><td>Project.task(uid=0).ActualWork</td></tr>
   * </table>
   */
  public void export(
  ) throws ServiceException {
	SysLog.info("export begin", activityGroup.refMofId());
    this.preExport();
    this.w("<?xml version=\"1.0\"  encoding=\"UTF-8\"?>");
    this.weba("Project", "xmlns=\"http://schemas.microsoft.com/project\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://schemas.microsoft.com/project mspdi.xsd\"");
    
    if (this.activityGroup.getName() != null) {
        this.we("Title", activityGroup.getName());
    }
    
    this.we("Subject", activityGroup.getName());    
    this.we("CalendarUID", Integer.toString(mspCalendar.getUID()));
    this.we("DefaultTaskType", Short.toString(mspTaskType));
    this.we("NewTasksEffortDriven", "1");
    this.we("TaskUpdatesResource", "0");

    this.exportExtendedAttributes();
    this.exportCalendars();
    if (exportActivities) {
        this.exportTasks();
    }
    if (exportResources) {
        this.exportResources();
    }
    if (exportAssignments) {
        this.exportAssignments();
    }
    
    //----
    //to calculate:
    //  <StartDate>2004-01-01T08:00:00</StartDate>
    //  <FinishDate>2004-05-13T15:00:00</FinishDate>
    //----
    this.wee("Project");
    this.postExport();
    SysLog.info("export end", activityGroup.getName());
  }

  //-----------------------------------------------------------------------
  private void preExport(
  ) throws ServiceException {
    this.clear();
    //Fill activityMappers
    //get all Activities
    Collection<Activity> activities = activityGroup.getFilteredActivity();
    for (Activity a: activities) {
      //if (!a.getName().startsWith("XTask")) continue;
      //read again to get obj with composite path.
      a = (Activity)pm.getObjectById(a.refGetPath());
      ActivityMapper am = new ActivityMapper(a);
      activityMappers.put(am.getActivity().getIdentity(),am);
    }
    //get all links of interest
    for (Iterator<ActivityMapper> i=activityMappers.values().iterator(); i.hasNext(); ) {
      this.setActivityLinks(i.next());
    }
    //set uid used during export
    for (Iterator<ActivityMapper> i=activityMappers.values().iterator(); i.hasNext(); ) {
     ActivityMapper am = i.next();
     if (am.isRootActivity()) {
       this.setActivityMapperUid(am, 0);
     }
    }
    
    //Fill resourceMappers
    int resourceUid = 0;
    for (Iterator<ActivityMapper> i=activityMappers.values().iterator(); i.hasNext(); ) {
      Activity a = (i.next()).getActivity();
      Collection<ResourceAssignment> assignments = a.getAssignedResource();
      for (ResourceAssignment assignment: assignments) {
        Resource r = assignment.getResource();
        if (r != null && !resourceMappers.containsKey(r.getIdentity())) {
          resourceUid++;
          resourceMappers.put(r.getIdentity(), new ResourceMapper(r,resourceUid));
        }
      }
    }
    
  }

  //-----------------------------------------------------------------------
  private void setActivityMapperUid (
      ActivityMapper am, 
      int deep
  ) throws ServiceException {
    if (deep >= MAX_ACTIVITY_LINKS) {
      throw new ServiceException(
          BasicException.Code.DEFAULT_DOMAIN,
          BasicException.Code.GENERIC,
          "Activities seems to be linked recursively!"
      );
    }
    am.setUid(nextUid);
    this.nextUid++;
    int newDeep = deep + 1;
    for (Iterator<ActivityMapper> i=am.getPartActivityMappers().iterator(); i.hasNext();) {
      ActivityMapper next = i.next();
      this.setActivityMapperUid(next, newDeep);
    }
  }

  //-----------------------------------------------------------------------
  private void setActivityLinks(
      ActivityMapper am
  ) {
	Collection<ActivityLinkTo> links = am.getActivity().getActivityLinkTo();
    for (ActivityLinkTo link: links) {
      if (link.getActivityLinkType() == isPartOf || 
          link.getActivityLinkType() == isContainerOf ||
          link.getActivityLinkType() == isChildOf ||
          link.getActivityLinkType() == isParentOf) {
        Activity linkedActivity = link.getLinkTo();
        if (linkedActivity == null) {
        	SysLog.warning("Activity "+ am.getActivity().getIdentity() + " has an ActivityLinkTo that doesn't reference a linked activity");
        }
        else {
          ActivityMapper linkedMapper = activityMappers.get(linkedActivity.getIdentity());
          if (linkedMapper == null) {
        	  SysLog.warning("Activity "+ am.getActivity().getIdentity() + " has a link to an Activity outside the exported set", linkedActivity.getIdentity());
          }
          else {
            if (link.getActivityLinkType() == isPartOf) {
              //linkedActivity is Container
              linkedMapper.addPartActivityMapper(am);            
            }
            else if (link.getActivityLinkType() == isContainerOf) {
              //source activiy is Container
              am.addPartActivityMapper(linkedMapper);
            }
            else if (link.getActivityLinkType() == isChildOf) {
              //linkedActivity is the parent
              am.addPredecessorActivityMapper(linkedMapper);
            }
            else if (link.getActivityLinkType() == isParentOf) {
              //source activiy is the Parent
              linkedMapper.addPredecessorActivityMapper(am);
            }
          }
        }
      }
    }
  }
  
  //-----------------------------------------------------------------------
  private void postExport(
  ) {
    this.clear();
    pw.flush();
  }

  //-----------------------------------------------------------------------
  private void clear(
  ) {
    this.initIndent();
    this.nextUid = 1;
    this.activityMappers.clear();
    this.resourceMappers.clear();
    if(this.exportAssignments) {
      this.exportActivities = true;
      this.exportResources = true;
    }
  }
  
  //-----------------------------------------------------------------------
  private void exportExtendedAttributes(
  ) {
    this.web("ExtendedAttributes");

    this.web("ExtendedAttribute");
    this.we("FieldID", ID_EXTENDED_TASK_ATTR_IDENTITY);
    this.we("FieldName", "Text1");
    this.we("Alias", "opencrx.identity");
    this.wee("ExtendedAttribute");

    this.web("ExtendedAttribute");
    this.we("FieldID", ID_EXTENDED_RESOURCE_ATTR_IDENTITY);
    this.we("FieldName", "Text1");
    this.we("Alias", "opencrx.identity");
    this.wee("ExtendedAttribute");

    this.wee("ExtendedAttributes");
  }

  //-----------------------------------------------------------------------
  private void exportCalendars(
  ) {
    //define the calendar to use. Use a 8h every day is a work day calendar
    this.web("Calendars");
    this.w(mspCalendar.toXml());
    this.wee("Calendars");
  }
  
  //-----------------------------------------------------------------------
  private void exportTasks(
  ) {
    this.web("Tasks");
    //task with uid 0 is a special task containing project infos.
    this.exportTask0();
    
    List<Integer> outlineNumber = new ArrayList<Integer>();
    outlineNumber.add(new Integer(0));
    for (Iterator<ActivityMapper> i=activityMappers.values().iterator(); i.hasNext(); ) {
     ActivityMapper am = i.next();
     if (am.isRootActivity()) {
       outlineNumber.set(0, new Integer((outlineNumber.get(0)).intValue() + 1)); 
       this.exportTask(am,outlineNumber);
     }
    }
    this.wee("Tasks");
  }
  
  //-----------------------------------------------------------------------
  private void exportTask0(
  ) {
    this.web("Task");
    this.we("UID" , "0");
    this.we("ID" , "0");
    if (activityGroup.getName() != null) {
        this.we("Name", activityGroup.getName());
    }
    //we("Type", "1");
    this.we("FixedCostAccrual", "3"); //if not present msproject displays an import error that the value is not valid.
    if (activityGroup.getDescription() != null) {
        this.we("Notes", activityGroup.getDescription());
    }

    //possible not necessary: MSProject calculates the actual effort
    //we("ActualWork", toMSProjectDuration(tracker.getActualEffortHours(), tracker.getActualEffortMinutes()));
    //String work = toMSProjectDuration(tracker.getSumEstimateEffortHours(), tracker.getSumEstimateEffortMinutes());
    //we("Work", work);
    //we("RegularWork", work);
    this.wee("Task");
  }
  
  //-----------------------------------------------------------------------
  /* EXAMPLE
    <Task>
      <UID>1</UID>
      <ID>1</ID>
      <Name>task1</Name>
      <Type>0</Type>
      <CreateDate>2006-05-08T09:59:00</CreateDate>
      <OutlineNumber>1</OutlineNumber>
      <OutlineLevel>1</OutlineLevel>
      <Priority>500</Priority>
      <Start>2006-05-09T00:00:00</Start>
      <Finish>2006-05-12T23:59:59</Finish>
      <DurationFormat>39</DurationFormat>
      <ActualDuration>PT40H0M0S</ActualDuration>
      <PercentComplete>80</PercentComplete>
      <ResumeValid>1</ResumeValid>
      <EffortDriven>1</EffortDriven>
      <ActualStart>2006-05-09</ActualStart>
      <ActualFinish>2006-05-09</ActualFinish>
      <ConstraintType>2</ConstraintType>
      <CalendarUID>1</CalendarUID>
      <ConstraintDate>2006-05-09</ConstraintDate>
      <IgnoreResourceCalendar>1</IgnoreResourceCalendar>
      <Baseline>
        <Number>0</Number>
        <Work>PT13H0M0S</Work>
      </Baseline>
    </Task>
   */
  private void exportTask(
      ActivityMapper am, 
      List<Integer> outlineNumber
  ) {

    Activity a = am.getActivity();
    
    //////////////////////////////////////
    // Step 1: Get values to export
    //////////////////////////////////////

    Date start = a.getScheduledStart();
    Date finish = a.getScheduledEnd();
    String durationFormat = "39"; //39 = d? (?=estimated), 7 = d

    int percentComplete = 
        a.getPercentComplete() == null ? 0 : a.getPercentComplete().intValue();
    int workingMinutesDuringScheduledPeriod = this.mspCalendar.getWorkingMinutes(
        a.getScheduledStart() == null ? new Date() : a.getScheduledStart(),
        a.getScheduledEnd() == null ? new Date() : a.getScheduledEnd()
    );
    int mainEstimateMinutes =
        (a.getMainEstimateEffortHours() == null ? 0 : a.getMainEstimateEffortHours().intValue() * 60) +
        (a.getMainEstimateEffortMinutes() == null ? 0 : a.getMainEstimateEffortMinutes().intValue());
    if(mainEstimateMinutes == 0) {
        mainEstimateMinutes = workingMinutesDuringScheduledPeriod;
    }
    // !useActualEffort --> derive actual duration from main estimate and percentComplete
    int actualDurationMinutes = (percentComplete * mainEstimateMinutes) / 100;
          
    Date earlyStart = a.getScheduledStart();
    Date earlyFinish = a.getScheduledEnd();
    Date lateStart = a.getActualStart();
    Date lateFinish = a.getActualEnd();
    Date actualStart = a.getActualStart();
    Date actualFinish = a.getActualEnd();
    int effortDriven = 1; //1=true, 0=false
    
    int uid = am.getUid();
    int id = am.getUid();
    String name = 
        "#" + a.getActivityNumber() + ": " +         
        (a.getName() == null ? "" : a.getName());
    String outlineNo = ProjectExporter.outlineNumberToString(outlineNumber);
    int outlineLevel = outlineNumber.size();
    short priority = a.getPriority();
    Date createDate = a.getCreatedAt();
    String notes = this.toMsProjectNotes(a.getDescription(), a.getDetailedDescription());
    
    ////////////////////////////////////////////
    // Step 2: recalculate some values to export
    ////////////////////////////////////////////

    if (actualStart != null) {
      start = actualStart;
    }
    if (actualFinish != null) {
      finish = actualFinish;
    }
    if (actualFinish != null) {
      durationFormat = "7";
    }
    am.setTaskAttributes(
        start, 
        finish, 
        actualFinish 
    );

    //////////////////////////////////
    //Step 3: export calculated values
    //////////////////////////////////
    
    this.web("Task");
    this.we("UID" , Integer.toString(uid));
    this.we("ID" , Integer.toString(id));
    if (name != null) {
        this.we("Name", name);
    }
    this.we("Type", Short.toString(mspTaskType));
    if (createDate != null) {
        this.we("CreateDate", this.toMsProjectDate(createDate));
    }
    if (outlineNo != null) {
        this.we("OutlineNumber",outlineNo);
    }
    this.we("OutlineLevel", Integer.toString(outlineLevel));
    this.we("Priority", Short.toString(priority));
    if (start != null) {
        this.we("Start", this.toFormatedStartDate(start));
    }
    if (finish != null) {
        this.we("Finish", this.toFormatedEndDate(finish));
    }
    this.we("DurationFormat", durationFormat);
    this.we("EffortDriven", Integer.toString(effortDriven));
    //we("Milestone", "0");
    //we("Summary",(am.getPartActivityMappers().size()>0 ? "1" : "0")); //wird offenbar anhand von outline berechnet.
    if (earlyStart != null) {
        this.we("EarlyStart", this.toFormatedStartDate(earlyStart));
    }
    if (earlyFinish != null) {
        this.we("EarlyFinish", this.toFormatedEndDate(earlyFinish));
    }
    if (lateStart != null) {
        this.we("LateStart", this.toFormatedStartDate(lateStart));
    }
    if (lateFinish != null) {
        this.we("LateFinish", this.toFormatedEndDate(lateFinish));
    }
    this.we("FixedCostAccrual", "3"); //if not present msproject displays an import error that the value is not valid.
    if (actualStart != null) {
        this.we("ActualStart", this.toFormatedStartDate(actualStart));
    }
    if (actualFinish != null) {
        this.we("ActualFinish", this.toFormatedEndDate(actualFinish));
    }
    this.we("PercentComplete", Integer.toString(percentComplete));        
    
    // Actual duration be reported relative to the working duration of the
    // standard calendar, i.e. the actual duration must be normalized to 
    // (workingHoursDuringScheduledPeriod / mainEstimateMinutes)
    int normalizedActualDurationMinutes = 
        (actualDurationMinutes * workingMinutesDuringScheduledPeriod) / 
        (mainEstimateMinutes == 0 ? 1 : mainEstimateMinutes);
    normalizedActualDurationMinutes = Math.abs(normalizedActualDurationMinutes);
    this.we("ActualDuration", ProjectExporter.toMSProjectDuration(normalizedActualDurationMinutes / 60, normalizedActualDurationMinutes % 60));
    
    this.we("CalendarUID", Integer.toString(this.mspCalendar.getUID()));
    this.exportHyperlink(a.getIdentity());
    this.we("IgnoreResourceCalendar", "true");
    this.we("Notes", notes);
    for (Iterator<ActivityMapper> i=am.getPredecessorActivityMappers().iterator(); i.hasNext();) {
        this.exportPredecessorLink(i.next());
    }
    this.exportExtendedAttribute(ID_EXTENDED_TASK_ATTR_IDENTITY, a.getIdentity());
    this.wee("Task");

    int lastOutlineNumberIx =outlineNumber.size();
    int lastOutlineNumber = 0;
    outlineNumber.add(lastOutlineNumberIx, new Integer(lastOutlineNumber));
    for (Iterator<ActivityMapper> i=am.getPartActivityMappers().iterator(); i.hasNext();) {
        lastOutlineNumber++;
        outlineNumber.set(lastOutlineNumberIx, new Integer(lastOutlineNumber));
        this.exportTask(i.next(), outlineNumber);
    }
    outlineNumber.remove(lastOutlineNumberIx);
  }
  
  //-----------------------------------------------------------------------
  private void exportPredecessorLink(
      ActivityMapper predecessor
  ) {
    this.web("PredecessorLink");
    this.we("PredecessorUID", Integer.toString(predecessor.getUid()));
    this.wee("PredecessorLink");
  }
  
  //-----------------------------------------------------------------------
  private void exportResources(
  ) {
    this.web("Resources");
    this.exportResource0();
    //order list using uid starting with 1;
    ResourceMapper[] resourceMappersArr = new ResourceMapper[this.resourceMappers.size()];
    for (Iterator<ResourceMapper> i=resourceMappers.values().iterator(); i.hasNext(); ) {
      ResourceMapper rm = i.next();
      resourceMappersArr[rm.getUid()-1] = rm; 
    }
    for (int i=0; i<resourceMappersArr.length; i++) {
      this.exportResource(resourceMappersArr[i]);
    }
    this.wee("Resources");
  }
  
  //-----------------------------------------------------------------------
  private void exportResource0(
  ) {
    this.web("Resource");
    this.we("UID" , "0");
    this.we("ID" , "0");
    this.wee("Resource");
  }

  //-----------------------------------------------------------------------
  private void exportResource(
      ResourceMapper rm
  ) {
    Resource r = rm.getResource();
    
    //////////////////////////////////////
    // Step 1: Initialize values to export
    //////////////////////////////////////
    int uid = rm.getUid();
    int id = rm.getUid();
    String name = r.getName() == null ? null : r.getName();
    String type = "1";  //0=Material, 1=Work (use only work-resources (no material resources supported)
    String notes = r.getDescription() == null ? null : r.getDescription();

    ////////////////////////////////////////////
    // Step 2: recalculate some values to export
    ////////////////////////////////////////////

    //////////////////////////////////
    //Step 3: export calculated values
    //////////////////////////////////
    this.web("Resource");
    this.we("UID" , Integer.toString(uid));
    this.we("ID" , Integer.toString(id));
    if (name != null) {
        this.we("Name", name);
    }
    if (type != null) {
        this.we("Type", type);
    }
    this.exportHyperlink(r.getIdentity());
    if (notes != null) {
        this.we("Notes", notes);
    }
    this.exportExtendedAttribute(ID_EXTENDED_RESOURCE_ATTR_IDENTITY, r.getIdentity());
    this.wee("Resource");
  }

  //-----------------------------------------------------------------------
  private void exportAssignments(
  ) {
    ResourceAssignmentQuery filter = null;
    this.web("Assignments");
    int assignmentUid = 0;
    for (Iterator<ActivityMapper> i=activityMappers.values().iterator(); i.hasNext(); ) {
      ActivityMapper am = i.next();
      Activity a = am.getActivity();
      if (filter == null) {
        Activity1Package pkg = (Activity1Package)a.refImmediatePackage();
        filter = pkg.createResourceAssignmentQuery();
        filter.orderByResourceOrder().ascending();
      }
      List<AssignmentMapper> assignmentMappers = new ArrayList<AssignmentMapper>();
      int totalWorkPercentage = 0;
      List<ResourceAssignment> assignments = a.getAssignedResource(filter); 
      for (ResourceAssignment ra: assignments) {
        if (ra.getWorkDurationPercentage() == null || ra.getWorkDurationPercentage().shortValue() < 0) {
          totalWorkPercentage = totalWorkPercentage + 100; //default is 100%
        }
        else {
          totalWorkPercentage = totalWorkPercentage + ra.getWorkDurationPercentage().shortValue();
        }
        assignmentUid++;
        assignmentMappers.add(new AssignmentMapper(assignmentUid, ra, am, resourceMappers.get(ra.getResource().getIdentity())));
      }
      if (totalWorkPercentage == 0) {
        totalWorkPercentage = 100 * assignmentMappers.size();
      }
      //export
      for (int j=0; j<assignmentMappers.size(); j++) {
        AssignmentMapper assignmentMapper = assignmentMappers.get(j);
        this.exportAssignment(
            assignmentMappers.size(), 
            assignmentMapper
        );
      }
    }
    this.wee("Assignments");
  }
  
  //-----------------------------------------------------------------------
  private void exportAssignment(
      int noOfAssignments, 
      AssignmentMapper assignment
  ) {
    this.web("Assignment");
    this.we("UID", Integer.toString(assignment.getUid()));
    this.we("TaskUID", Integer.toString(assignment.getTaskUid()));
    this.we("ResourceUID", Integer.toString(assignment.getResourceUid()));
    this.we("ActualWork", ProjectExporter.toMSProjectDuration(assignment.getActualWorkHours(),0)); //used if task type = fixed units
    if (assignment.getFinish() != null) {
        this.we("Finish", this.toMsProjectDate(assignment.getFinish()));
    }
    this.exportHyperlink(assignment.getResourceAssignment().getIdentity());
    this.we("Units", Double.toString((1.0 * assignment.getWorkDurationPercentage()) / 100.0));

    this.web("TimephasedData");
    this.we("Type", "2"); //2=assignment actual work, 1=assignment remaining work
    this.we("UID", "2"); //it's allowed here to always export same number.
    if (assignment.getStart() != null) {
        this.we("Start", this.toMsProjectDate(assignment.getStart()));
    }
    if (assignment.getFinish() != null) {
        this.we("Finish", this.toMsProjectDate(assignment.getFinish()));
    }
    this.we("Unit", "2"); //The time unit of the timephased data period. Values are: 0=m, 1=h, 2=d, 3=w, 5=mo, 8=y
    this.we("Value", ProjectExporter.toMSProjectDuration(assignment.getActualWorkHours(),0));
    this.wee("TimephasedData");

    this.wee("Assignment");
  }

    //-----------------------------------------------------------------------
    private void exportHyperlink(
        String identity
    ) {
        this.we("Hyperlink", "hyperlink to corresponding openCRX object");
        if(this.mspHyperlinkRootAddress == null) {
            this.we("HyperlinkAddress", identity);
        }
        else {
            Action action = 
                new Action(
                    SelectObjectAction.EVENT_ID, 
                    new Action.Parameter[]{
                        new Action.Parameter(Action.PARAMETER_OBJECTXRI, identity)
                    },
                    "",
                    true
                );
            this.we("HyperlinkAddress", mspHyperlinkRootAddress + "/" + action.getParameter());
        }
    }
  
  //-----------------------------------------------------------------------
  private static String outlineNumberToString(
      List<Integer> outlineNumber
  ) {
    StringBuffer buf = new StringBuffer();
    for (int i=0; i<outlineNumber.size(); i++) {
      if (buf.length()>0) {
        buf.append('.');
      }
      buf.append(outlineNumber.get(i));
    }
    return buf.toString();
  }

  //-----------------------------------------------------------------------
  private String toMsProjectDate(
      Date d
  ) {
    return this.projectDateFormatter.format(d);
  }

  //-----------------------------------------------------------------------
  private String toMsProjectNotes(
      String description, 
      String detail
  ) {
    StringBuffer b = new StringBuffer();
    if (description != null) {
      b.append(description);
      b.append(LINE_SEPARATOR);
      b.append(LINE_SEPARATOR);
    }
    if (detail != null) {
      b.append(detail);
    }
    return b.toString();
  }
  
  //-----------------------------------------------------------------------
  private void exportExtendedAttribute(
      String fieldID, 
      String value
  ) {
      this.web("ExtendedAttribute");
      this.we("FieldID", fieldID);
      this.we("Value", value);
      this.wee("ExtendedAttribute");
  }
  
  //-----------------------------------------------------------------------
  /**
   * Writes a String.
   */
  private void w(
      String stringToExport
  ) {
    this.pw.print(this.leadingSpaces);
    this.pw.println(stringToExport);
  }
  
  //-----------------------------------------------------------------------
  /**
   * Writes an element with its value.
   */
  private void we(
      String element, 
      String value
  ){
    this.pw.print(leadingSpaces);
    this.pw.print("<");
    this.pw.print(element);
    this.pw.print(">");
    this.pw.print(XMLEncoder.encode(value));
    this.pw.print("</");
    this.pw.print(element);
    this.pw.println(">");
  }
  
  //-----------------------------------------------------------------------
  /**
   * Writes an element begin.
   */
  private void web(
      String element
  ){
    this.pw.print(this.leadingSpaces);
    this.pw.print("<");
    this.pw.print(element);
    this.pw.println(">");
    this.incrementIndent();
  }

  //-----------------------------------------------------------------------
  /**
   * Writes an element begin with attributes.
   */
  private void weba(
      String element, 
      String attributes
  ){
    this.pw.print(leadingSpaces);
    this.pw.print("<");
    this.pw.print(element);
    this.pw.print(" ");
    this.pw.print(attributes);
    this.pw.println(">");
    this.incrementIndent();
  }

  //-----------------------------------------------------------------------
  /**
   * Writes an element end.
   */
  private void wee(
      String element
  ) {
    this.decrementIndent();
    this.pw.print(leadingSpaces);
    this.pw.print("</");
    this.pw.print(element);
    this.pw.println(">");
  }

  //-----------------------------------------------------------------------
  private void incrementIndent(
  ) {
    this.leadingSpaces = this.leadingSpaces + "  ";
  }
  //-----------------------------------------------------------------------
  private void decrementIndent(
  ) {
    this.leadingSpaces = this.leadingSpaces.substring(
        0, 
        this.leadingSpaces.length() - 2
    );
  }
  //-----------------------------------------------------------------------
  private void initIndent(
  ) {
    this.leadingSpaces = "";
  }

  //-----------------------------------------------------------------------
  public void close(
  ) throws Throwable {
      if(this.pw != null) {
          this.pw.close();
      }
  }

  //-----------------------------------------------------------------------
  protected void finalize(
  ) throws Throwable {
    this.close();
    super.finalize();
  }

  //-----------------------------------------------------------------------
  private static String toMSProjectDuration(int hours, int minutes) {
    return "PT" + hours + "H" + minutes + "M0S";
  }

  //-------------------------------------------------------------------------
  // Variables
  //-------------------------------------------------------------------------
  private static final String LINE_SEPARATOR = System.getProperty("line.separator");
  private static final int MAX_ACTIVITY_LINKS = 10;

  public static final short  TASK_TYPE_FIXED_UNITS = 0;
  public static final short  TASK_TYPE_FIXED_DURATION = 1;
  public static final short  TASK_TYPE_FIXED_WORK = 2;
  public static final String ID_EXTENDED_TASK_ATTR_IDENTITY = "188743731";
  public static final String ID_EXTENDED_TASK_ATTR_2 = "188743734";
  public static final String ID_EXTENDED_TASK_ATTR_3 = "188743737";
  public static final String ID_EXTENDED_RESOURCE_ATTR_IDENTITY = "205520904";
  public static final String ID_EXTENDED_RESOURCE_ATTR_2 = "205520905";
  public static final String ID_EXTENDED_RESOURCE_ATTR_3 = "205520926";

  private DateFormat startDateFormater = new SimpleDateFormat("yyyy-MM-dd'T00:00:00'");
  private DateFormat endDateFormater = new SimpleDateFormat("yyyy-MM-dd'T23:59:59'");
  
  private PrintWriter pw;
  private ActivityGroup activityGroup;
  private PersistenceManager pm;
  private String mspHyperlinkRootAddress = null;
  private boolean exportActivities = true;
  private boolean exportResources = true;
  private boolean exportAssignments = true;
  private String leadingSpaces = ""; //current leadingSpaces to format XML export with leading spaces
  private int nextUid = 1; //next msproject uid to use.
  private short isParentOf = 1; //predecessor
  private short isChildOf = 99; //follower 
  private short isPartOf = 95; //detail of a summary
  private short isContainerOf = 101; //summary has details // TODO define code in config
  private short mspTaskType = TASK_TYPE_FIXED_UNITS; //= default msproject task type
  private DateFormat projectDateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss");
  private StandardCalendar mspCalendar = new StandardCalendar();
  private Map<String,ActivityMapper> activityMappers = new HashMap<String,ActivityMapper>(); //key is identity of contained Activity
  private Map<String,ResourceMapper> resourceMappers = new HashMap<String,ResourceMapper>(); //key is identity of contained Resource
  
}

// --- End of File -----------------------------------------------------------
