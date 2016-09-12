/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ManageGUIPermissionsWizardController
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
package org.opencrx.kernel.portal.wizard;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.backend.Admin;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.SecureObject;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.WebKeys;
import org.openmdx.portal.servlet.component.AttributePane;
import org.openmdx.portal.servlet.component.UiAttributeTab;
import org.openmdx.portal.servlet.component.UiFieldGroup;
import org.openmdx.portal.servlet.component.ObjectView;
import org.openmdx.portal.servlet.component.ReferencePane;
import org.openmdx.portal.servlet.component.ShowObjectView;
import org.openmdx.portal.servlet.control.UiFieldGroupControl;
import org.openmdx.portal.servlet.control.UiGridControl;
import org.openmdx.portal.servlet.control.OperationPaneControl;
import org.openmdx.portal.servlet.control.UiOperationTabControl;
import org.openmdx.portal.servlet.control.WizardControl;
import org.openmdx.portal.servlet.control.UiWizardTabControl;

/**
 * ManageGUIPermissionsWizardController
 *
 */
public class ManageGUIPermissionsWizardController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public ManageGUIPermissionsWizardController(
	) {
		super();
	}
	
	/**
	 * Get generic permissions.
	 * 
	 * @param obj
	 * @param elementName
	 * @param hasEditAction
	 * @return
	 */
	public List<String> getGenericPermissions(
		RefObject_1_0 obj,
		String elementName,
		boolean hasEditAction
	) {
		List<String> permissions = new ArrayList<String>();
		if(elementName.indexOf(":") < 0 || !elementName.startsWith(obj.refClass().refMofId())) {
			permissions.add(elementName + "|" + WebKeys.PERMISSION_REVOKE_SHOW);
			permissions.add(elementName + "|" + WebKeys.PERMISSION_GRANT_SHOW);
			if(hasEditAction) {
				permissions.add(elementName + "|" + WebKeys.PERMISSION_REVOKE_EDIT);
				permissions.add(elementName + "|" + WebKeys.PERMISSION_GRANT_EDIT);
			}
		}
		return permissions;
	}

	/**
	 * Get specific permissions.
	 * 
	 * @param obj
	 * @param elementName
	 * @param hasEditAction
	 * @return
	 */
	public List<String> getSpecificPermissions(
		RefObject_1_0 obj,
		String elementName,
		boolean hasEditAction
	) {
		List<String> permissions = new ArrayList<String>();
		if(elementName.indexOf(":") > 0) {
			if(elementName.startsWith(obj.refClass().refMofId())) {
				permissions.add(elementName + "|" + WebKeys.PERMISSION_REVOKE_SHOW);
				permissions.add(elementName + "|" + WebKeys.PERMISSION_GRANT_SHOW);
				if(hasEditAction) {
					permissions.add(elementName + "|" + WebKeys.PERMISSION_REVOKE_EDIT);
					permissions.add(elementName + "|" + WebKeys.PERMISSION_GRANT_EDIT);
				}
			} else {
				String specificElementName = obj.refClass().refMofId() + elementName.substring(elementName.lastIndexOf(":"));
				permissions.add(specificElementName + "|" + WebKeys.PERMISSION_REVOKE_SHOW);
				if(hasEditAction) {
					permissions.add(specificElementName + "|" + WebKeys.PERMISSION_REVOKE_EDIT);
				}
				permissions.add(specificElementName + "|" + WebKeys.PERMISSION_GRANT_SHOW);
				if(hasEditAction) {
					permissions.add(specificElementName + "|" + WebKeys.PERMISSION_GRANT_EDIT);
				}
			}
		}
		return permissions;
	}
	
	/**
	 * Test whether permission is stored.
	 * 
	 * @param storedPermissions
	 * @param permission
	 * @return
	 */
	public boolean isStoredPermission(
		List<String> storedPermissions,
		String permission
	) {
		return storedPermissions.indexOf(permission) >= 0;
	}
	
	/**
	 * Add permission.
	 * 
	 * @param policy
	 * @param role
	 * @param permission
	 * @throws ServiceException
	 */
	public void addPermission(
		org.openmdx.security.authorization1.jmi1.Policy policy,
		org.openmdx.security.realm1.jmi1.Role role,
		String permission
	) throws ServiceException {
		String[] p = permission.split("\\|");
		if(p.length == 2) {
			javax.jdo.PersistenceManager pm = javax.jdo.JDOHelper.getPersistenceManager(role);
			org.openmdx.security.realm1.cci2.PermissionQuery permissionQuery = (org.openmdx.security.realm1.cci2.PermissionQuery)pm.newQuery(org.openmdx.security.realm1.jmi1.Permission.class);
			permissionQuery.name().equalTo(p[0]);
			permissionQuery.thereExistsAction().equalTo(p[1]);
			Collection<org.openmdx.security.realm1.jmi1.Permission> permissions = role.getPermission(permissionQuery);
			if(permissions.isEmpty()) {
				org.openmdx.security.realm1.cci2.PrivilegeQuery privilegeQuery = (org.openmdx.security.realm1.cci2.PrivilegeQuery)pm.newQuery(org.openmdx.security.realm1.jmi1.Privilege.class);
				privilegeQuery.name().equalTo(p[0]);
				List<org.openmdx.security.realm1.jmi1.Privilege> privileges = policy.getPrivilege(privilegeQuery);
				org.openmdx.security.realm1.jmi1.Privilege privilege = null;
				if(privileges.isEmpty()) {
					privilege = pm.newInstance(org.openmdx.security.realm1.jmi1.Privilege.class);
					privilege.setName(p[0]);
					policy.addPrivilege(
						Base.getInstance().getUidAsString(),
						privilege
					);
				} else {
					privilege = privileges.iterator().next();
				}
				org.openmdx.security.realm1.jmi1.Permission newPermission = pm.newInstance(org.openmdx.security.realm1.jmi1.Permission.class);
				newPermission.setName(p[0]);
				newPermission.getAction().add(p[1]);
				newPermission.setPrivilege(privilege);
				role.addPermission(
					Base.getInstance().getUidAsString(), 
					newPermission
				);
			}
		}
	}
	
	/**
	 * Remove permission.
	 * 
	 * @param role
	 * @param permission
	 */
	public void removePermission(
		org.openmdx.security.realm1.jmi1.Role role,
		String permission
	) {
		String[] p = permission.split("\\|");
		if(p.length == 2) {
			javax.jdo.PersistenceManager pm = javax.jdo.JDOHelper.getPersistenceManager(role);
			org.openmdx.security.realm1.cci2.PermissionQuery permissionQuery = (org.openmdx.security.realm1.cci2.PermissionQuery)pm.newQuery(org.openmdx.security.realm1.jmi1.Permission.class);
			permissionQuery.name().equalTo(p[0]);
			permissionQuery.thereExistsAction().equalTo(p[1]);
			Collection<org.openmdx.security.realm1.jmi1.Permission> permissions = role.getPermission(permissionQuery);
			for(org.openmdx.security.realm1.jmi1.Permission permissionToBeRemoved: permissions) {
				pm.deletePersistent(permissionToBeRemoved);
			}
		}
	}

	/**
	 * WizardViewState
	 *
	 */
	public static class WizardViewState {
		
		// Generic permissions
		public List<String> genericPermissions = new ArrayList<String>();
		public List<String> removedGenericPermissions = new ArrayList<String>();
		public List<String> addedGenericPermissions = new ArrayList<String>();
		public List<String> currentGenericPermissions = new ArrayList<String>();

		// Specific permissions
		public List<String> specificPermissions = new ArrayList<String>();
		public List<String> removedSpecificPermissions = new ArrayList<String>();
		public List<String> addedSpecificPermissions = new ArrayList<String>();
		public List<String> currentSpecificPermissions = new ArrayList<String>();		
	}
	
	/**
	 * WizardState
	 *
	 */
	public static class WizardState {
		
		public static String VIEW_OPERATION_PERMISSIONS = "Operations";
		public static String VIEW_FIELD_PERMISSIONS = "Fields";
		public static String VIEW_GRID_PERMISSIONS = "Grids";
		
		public WizardState(
		) {
			viewStates.put(VIEW_OPERATION_PERMISSIONS, new WizardViewState());
			viewStates.put(VIEW_FIELD_PERMISSIONS, new WizardViewState());
			viewStates.put(VIEW_GRID_PERMISSIONS, new WizardViewState());
		}
		
		public boolean isDirty(
		) {
			for(WizardViewState viewState: viewStates.values()) {
				if(
					!viewState.addedGenericPermissions.isEmpty() ||
					!viewState.addedSpecificPermissions.isEmpty() ||
					!viewState.removedGenericPermissions.isEmpty() ||
					!viewState.removedSpecificPermissions.isEmpty()
				) {
					return true;
				}
			}
			return false;
		}
		
		/**
		 * @return the objectIdentity
		 */
		public Path getObjectIdentity() {
			return objectIdentity;
		}

		/**
		 * @param objectIdentity the objectIdentity to set
		 */
		public void setObjectIdentity(Path objectIdentity) {
			this.objectIdentity = objectIdentity;
		}

		/**
		 * @return the roleName
		 */
		public String getRoleName() {
			return roleName;
		}

		/**
		 * @param roleName the roleName to set
		 */
		public void setRoleName(String roleName) {
			this.roleName = roleName;
		}

		/**
		 * @return the viewStates
		 */
		public Map<String, WizardViewState> getViewStates() {
			return viewStates;
		}

		private Path objectIdentity = null;
		private String roleName = null;
		private final Map<String,WizardViewState> viewStates = new HashMap<String,WizardViewState>();
		
	}

	/**
	 * Cancel action.
	 * 
	 */
	public void doCancel(		
	) {
		this.getSession().removeAttribute(
			WizardState.class.getName()
		);
		this.setExitAction(
			new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);
	}

	/**
	 * Refresh action.
	 * 
	 * @param storePermissions
	 * @param viewName
	 * @param roleName
	 * @throws ServiceException
	 */
	public void doRefresh(
		@RequestParameter(name = "storePermissions") Boolean storePermissions,
		@RequestParameter(name = "view") String viewName,
		@RequestParameter(name = "role") String roleName
	) throws ServiceException {
		RefObject_1_0 obj = this.getObject();
		this.viewName = viewName;
		if(this.viewName == null) {
			this.viewName = WizardState.VIEW_FIELD_PERMISSIONS;
		}
		this.roleName = roleName;
		if(this.roleName == null) {
			this.roleName = "Public";
		}
		PersistenceManager pm = this.getPm();
		org.openmdx.security.realm1.jmi1.Principal currentPrincipal = Utils.getRequestingPrincipal(pm, this.getProviderName(), this.getSegmentName());
		this.currentUserIsAdmin = currentPrincipal.refGetPath().getBase().equals(SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + this.getSegmentName());
		this.wizardState = (WizardState)this.getSession().getAttribute(
			WizardState.class.getName()
		);	
		this.policy = null;
		try {
			this.policy = SecureObject.getInstance().getPolicy(
				pm,
				this.getProviderName(),
				this.getSegmentName()
			);
		} catch(Exception e) {}
		if(this.currentUserIsAdmin && this.policy == null) {
			// Create segment-specific policy if it does not exist already
			pm.currentTransaction().begin();
			this.policy = Admin.getInstance().createPolicy(
				pm, 
				this.getProviderName(), 
				this.getSegmentName()
			);
			// Create role Public so at least one role is shown in role drop-down
			org.openmdx.security.realm1.jmi1.Role role = pm.newInstance(org.openmdx.security.realm1.jmi1.Role.class);
			role.setName("Public");
			role.setDescription(this.getSegmentName() + "\\\\Public");
			this.policy.addRole(
				"Public",
				role
			);
			pm.currentTransaction().commit();
		}
		org.openmdx.security.realm1.jmi1.Role selectedRole = null;
		if(this.policy != null) {
			org.openmdx.security.realm1.cci2.RoleQuery roleQuery = (org.openmdx.security.realm1.cci2.RoleQuery)pm.newQuery(org.openmdx.security.realm1.jmi1.Role.class);
			roleQuery.name().equalTo(this.roleName);
			List<org.openmdx.security.realm1.jmi1.Role> roles = this.policy.getRole(roleQuery);
			if(!roles.isEmpty()) {
				selectedRole = roles.iterator().next();							
			}
		}
		// Store permissions
		if(this.wizardState != null && Boolean.TRUE.equals(storePermissions)) {
			String[] views = {
				WizardState.VIEW_FIELD_PERMISSIONS, 
				WizardState.VIEW_GRID_PERMISSIONS, 
				WizardState.VIEW_OPERATION_PERMISSIONS
			};
			pm.currentTransaction().begin();
			for(String view: views) {
				WizardViewState viewState = this.wizardState.getViewStates().get(view);
				for(String permission: viewState.addedGenericPermissions) {
					this.addPermission(
						this.policy, 
						selectedRole, 
						permission
					);
				}
				for(String permission: viewState.removedGenericPermissions) {
					this.removePermission(
						selectedRole, 
						permission
					);
				}
				for(String permission: viewState.addedSpecificPermissions) {
					this.addPermission(
						this.policy, 
						selectedRole, 
						permission
					);
				}
				for(String permission: viewState.removedSpecificPermissions) {
					this.removePermission(
						selectedRole, 
						permission
					);
				}
			}
			pm.currentTransaction().commit();
			this.wizardState = null;			
		}
		// Update wizard state
		if(
			this.wizardState == null ||
			!this.roleName.equals(this.wizardState.getRoleName()) ||
			!obj.refGetPath().equals(this.wizardState.getObjectIdentity())
		) {
			this.getSession().setAttribute(
				WizardState.class.getName(),
				this.wizardState = new WizardState()
			);
			this.wizardState.setRoleName(this.roleName);
			this.wizardState.setObjectIdentity(obj.refGetPath());
			if(selectedRole == null) {
				org.openmdx.security.realm1.cci2.RoleQuery roleQuery = (org.openmdx.security.realm1.cci2.RoleQuery)pm.newQuery(org.openmdx.security.realm1.jmi1.Role.class);
				roleQuery.orderByName().ascending();
				List<org.openmdx.security.realm1.jmi1.Role> roles = this.policy.getRole(roleQuery);
				if(!roles.isEmpty()) {
					selectedRole = roles.iterator().next();
				}
			}
			List<String> storedPermissions = new ArrayList<String>();
			if(selectedRole != null) {
				Collection<org.openmdx.security.realm1.jmi1.Permission> permissions = selectedRole.getPermission();
				for(org.openmdx.security.realm1.jmi1.Permission permission: permissions) {
					for(String action: permission.getAction()) {
						storedPermissions.add(permission.getPrivilege().getName() + "|" + action);
					}
				}
			}
			// Get ui elements
			ObjectView view = this.getCurrentView();
			if(view instanceof ShowObjectView) {
				ShowObjectView showView = (ShowObjectView)view;
				// Operations
				WizardViewState viewState = this.wizardState.viewStates.get(WizardState.VIEW_OPERATION_PERMISSIONS);
				for(OperationPaneControl paneControl: showView.getControl().getChildren(OperationPaneControl.class)) {
					for(UiOperationTabControl tabControl: paneControl.getChildren(UiOperationTabControl.class)) {
						String elementName = tabControl.getQualifiedOperationName();
						for(String permission: getGenericPermissions(this.getObject(), elementName, true)) {
							if(isStoredPermission(storedPermissions, permission)) {
								viewState.currentGenericPermissions.add(permission);
							} else {
								viewState.genericPermissions.add(permission);
							}
						}
						for(String permission: getSpecificPermissions(this.getObject(), elementName, true)) {
							if(isStoredPermission(storedPermissions, permission)) {
								viewState.currentSpecificPermissions.add(permission);
							} else {
								viewState.specificPermissions.add(permission);
							}										
						}
					}
				}
				// Wizards
				for(UiWizardTabControl tabControl: showView.getControl().getChildren(WizardControl.class).get(0).getChildren(UiWizardTabControl.class)) {
					for(String permission: getGenericPermissions(this.getObject(), tabControl.getQualifiedOperationName(), true)) {
						if(isStoredPermission(storedPermissions, permission)) {
							viewState.currentGenericPermissions.add(permission);
						} else {
							viewState.genericPermissions.add(permission);
						}									
					}
				}
				// Fields
				viewState = this.wizardState.viewStates.get(WizardState.VIEW_FIELD_PERMISSIONS);
				for(AttributePane attributePane: showView.getChildren(AttributePane.class)) {
					for(UiAttributeTab attributeTab: attributePane.getChildren(UiAttributeTab.class)) {
						// Attribute tabs only have specific permissions
						{
							String elementName = attributeTab.getControl().getId();
							for(String permission: getSpecificPermissions(this.getObject(), elementName, false)) {
								if(isStoredPermission(storedPermissions, permission)) {
									viewState.currentSpecificPermissions.add(permission);
								} else {
									viewState.specificPermissions.add(permission);
								}
							}
						}
						for(UiFieldGroup fieldGroup: attributeTab.getChildren(UiFieldGroup.class)) {
							// Field groups only have specific permissions
							{
								String elementName = fieldGroup.getControl().getId();
								for(String permission: getSpecificPermissions(this.getObject(), elementName, false)) {
									if(isStoredPermission(storedPermissions, permission)) {
										viewState.currentSpecificPermissions.add(permission);
									} else {
										viewState.specificPermissions.add(permission);
									}
								}
							}
							for(UiFieldGroupControl.Field field: fieldGroup.getFields()) {
								String elementName = field.getField().getQualifiedFeatureName();
								for(String permission: getGenericPermissions(this.getObject(), elementName, true)) {
									if(isStoredPermission(storedPermissions, permission)) {
										viewState.currentGenericPermissions.add(permission);
									} else {
										viewState.genericPermissions.add(permission);
									}
								}
								for(String permission: getSpecificPermissions(this.getObject(), elementName, true)) {
									if(isStoredPermission(storedPermissions, permission)) {
										viewState.currentSpecificPermissions.add(permission);
									} else {
										viewState.specificPermissions.add(permission);
									}
								}
							}
						}
					}
				}
				// Grids
				viewState = this.wizardState.viewStates.get(WizardState.VIEW_GRID_PERMISSIONS);
				for(ReferencePane referencePane: showView.getChildren(ReferencePane.class)) {
					for(UiGridControl gridControl: referencePane.getControl().getChildren(UiGridControl.class)) {
						String elementName = gridControl.getQualifiedReferenceTypeName();
						for(String permission: getGenericPermissions(this.getObject(), elementName, true)) {
							if(isStoredPermission(storedPermissions, permission)) {
								viewState.currentGenericPermissions.add(permission);
							} else {
								viewState.genericPermissions.add(permission);
							}
						}
						for(String permission: getSpecificPermissions(this.getObject(), elementName, true)) {
							if(isStoredPermission(storedPermissions, permission)) {
								viewState.currentSpecificPermissions.add(permission);
							} else {
								viewState.specificPermissions.add(permission);
							}
						}
					}
				}
			}
		}
	}

	/**
	 * Apply action.
	 * 
	 * @param viewName
	 * @param roleName
	 * @throws ServiceException
	 */
	public void doApply(
		@RequestParameter(name = "view") String viewName,
		@RequestParameter(name = "role") String roleName		
	) throws ServiceException {
		this.doRefresh(
			true, // storePermissions
			viewName, 
			roleName
		);
	}

	/**
	 * AddGenericPermissions action.
	 * 
	 * @param viewName
	 * @param roleName
	 * @param selectedPermissions
	 * @throws ServiceException
	 */
	public void doAddGenericPermissions(
		@RequestParameter(name = "view") String viewName,
		@RequestParameter(name = "role") String roleName,
		@RequestParameter(name = "genericPermissions") String[] selectedPermissions
	) throws ServiceException {
		this.doRefresh(
			false, // storePermissions 
			viewName, 
			roleName
		);
		if(this.wizardState != null && selectedPermissions != null) {
			WizardViewState viewState = this.wizardState.getViewStates().get(viewName);		
			viewState.genericPermissions.removeAll(Arrays.asList(selectedPermissions));
			viewState.removedGenericPermissions.removeAll(Arrays.asList(selectedPermissions));
			viewState.addedGenericPermissions.addAll(Arrays.asList(selectedPermissions));
			viewState.currentGenericPermissions.addAll(Arrays.asList(selectedPermissions));
		}
	}

	/**
	 * RemoveGenericPermissions action.
	 * 
	 * @param viewName
	 * @param roleName
	 * @param selectedPermissions
	 * @throws ServiceException
	 */
	public void doRemoveGenericPermissions(
		@RequestParameter(name = "view") String viewName,
		@RequestParameter(name = "role") String roleName,
		@RequestParameter(name = "currentGenericPermissions") String[] selectedPermissions
	) throws ServiceException {
		this.doRefresh(
			false, // storePermissions 
			viewName, 
			roleName
		);
		if(this.wizardState != null && selectedPermissions != null) {
			WizardViewState viewState = this.wizardState.getViewStates().get(viewName);		
			viewState.genericPermissions.addAll(Arrays.asList(selectedPermissions));
			viewState.removedGenericPermissions.addAll(Arrays.asList(selectedPermissions));
			viewState.addedGenericPermissions.removeAll(Arrays.asList(selectedPermissions));
			viewState.currentGenericPermissions.removeAll(Arrays.asList(selectedPermissions));
		}		
	}
	
	/**
	 * AddSpecificPermissions action.
	 * 
	 * @param viewName
	 * @param roleName
	 * @param selectedPermissions
	 * @throws ServiceException
	 */
	public void doAddSpecificPermissions(
		@RequestParameter(name = "view") String viewName,
		@RequestParameter(name = "role") String roleName,
		@RequestParameter(name = "specificPermissions") String[] selectedPermissions
	) throws ServiceException {
		this.doRefresh(
			false, // storePermissions 
			viewName, 
			roleName
		);
		if(this.wizardState != null && selectedPermissions != null) {
			WizardViewState viewState = this.wizardState.getViewStates().get(viewName);		
			viewState.specificPermissions.removeAll(Arrays.asList(selectedPermissions));
			viewState.removedSpecificPermissions.removeAll(Arrays.asList(selectedPermissions));
			viewState.addedSpecificPermissions.addAll(Arrays.asList(selectedPermissions));
			viewState.currentSpecificPermissions.addAll(Arrays.asList(selectedPermissions));
		}		
	}

	/**
	 * RemoveSpecificPermissions action.
	 * 
	 * @param viewName
	 * @param roleName
	 * @param selectedPermissions
	 * @throws ServiceException
	 */
	public void doRemoveSpecificPermissions(
		@RequestParameter(name = "view") String viewName,
		@RequestParameter(name = "role") String roleName,
		@RequestParameter(name = "currentSpecificPermissions") String[] selectedPermissions
	) throws ServiceException {
		this.doRefresh(
			false, // storePermissions 
			viewName, 
			roleName
		);
		if(this.wizardState != null && selectedPermissions != null) {
			WizardViewState viewState = this.wizardState.getViewStates().get(viewName);		
			viewState.specificPermissions.addAll(Arrays.asList(selectedPermissions));
			viewState.removedSpecificPermissions.addAll(Arrays.asList(selectedPermissions));
			viewState.addedSpecificPermissions.removeAll(Arrays.asList(selectedPermissions));
			viewState.currentSpecificPermissions.removeAll(Arrays.asList(selectedPermissions));
		}		
	}

	/**
	 * @return the viewName
	 */
	public String getViewName() {
		return viewName;
	}

	/**
	 * @return the roleName
	 */
	public String getRoleName() {
		return roleName;
	}

	/**
	 * @return the policy
	 */
	public org.openmdx.security.authorization1.jmi1.Policy getPolicy() {
		return policy;
	}

	/**
	 * @return the wizardState
	 */
	public WizardState getWizardState() {
		return wizardState;
	}

	/**
	 * @return the currentUserIsAdmin
	 */
	public boolean isCurrentUserIsAdmin() {
		return currentUserIsAdmin;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	private String viewName;
	private String roleName;
	private boolean currentUserIsAdmin;
	private WizardState wizardState;
	private org.openmdx.security.authorization1.jmi1.Policy policy;
}
