/*
 * ====================================================================
 * Project:     openCRX/Gradle, http://www.opencrx.org/
 * Description: ArchiveTask
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
package org.opencrx.gradle

import org.gradle.api.tasks.bundling.Jar
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.OutputDirectory
import org.gradle.api.tasks.InputDirectory
import org.gradle.api.tasks.InputFiles
import org.gradle.api.JavaVersion
import java.io.File
import java.io.FileInputStream
import java.util.Properties
import java.text.SimpleDateFormat
import java.util.TimeZone
import java.util.Date

open class ArchiveTask() : Jar() {

	@get:Input
    var env: Properties 
	init {
		env = Properties()
		env.load(FileInputStream(File(getProject().getRootDir(), "build.properties")))
		env.load(FileInputStream(File(getProject().getProjectDir(), "build.properties")))
	}

	@Internal val targetPlatform = JavaVersion.valueOf(env.getProperty("target.platform"))
	@Internal var providerName = env.getProperty("provider.name")
	@Internal var buildDir = project.layout.buildDirectory.dir("./")
	@Internal var projectDir = getProject().getProjectDir()
	@OutputDirectory var deliverDir = File("./build")
	@Internal var appSourceName = "opencrx-core-" + providerName
	@Internal var appTargetName = "opencrx-core-" + providerName
	@InputDirectory var dataHome = File(projectDir, "src/data") 
	@Internal var dataDir = env.getProperty("data.dir")
	@Internal var dataDirGeneric = env.getProperty("data.dir.generic") 
	@Internal var datasourceName = env.getProperty("datasource.name")
	@Internal var configurationClassName = env.getProperty("configuration.class.name")
	@Internal var kernelApplicationPluginClassName = env.getProperty("kernel.application.plugin.class.name")
	@Internal var kernelModelPluginClassName = env.getProperty("kernel.model.plugin.class.name")
	@Internal var kernelPersistencePluginClassName = env.getProperty("kernel.persistence.plugin.class.name")
	@Internal var securityApplicationPluginClassName = env.getProperty("security.application.plugin.class.name")
	@Internal var imapListenPort = env.getProperty("imap.listenPort")	
	@Internal var calendarGlobalInterfaceLanguage = env.getProperty("calendar.globalInterfaceLanguage")
	@Internal var calendarGlobalInterfaceCustomLanguages = env.getProperty("calendar.globalInterfaceCustomLanguages")
	
	@Input var projectImplementationVersion: String
	init {		
		projectImplementationVersion = project.getVersion().toString();
	}

	@Input var projectSpecificationVersion: String
	init {
		projectSpecificationVersion = project.getVersion().toString()
	}

	@Input var projectVendorId: String
	init {
		val v = env.getProperty("project.vendor.id");
		projectVendorId = if(v == null) "org.opencrx" else v;
	}

	@Input var projectVendorName: String
	init {
		val v = env.getProperty("project.vendor.name");
		projectVendorName = if(v == null) "openCRX" else v;
	}

	fun getWebAppName(name: String): String {
		return "opencrx-" + name + "-" + providerName
	}
	
	fun getManifest(
		specificationTitle: String,
		implementationTitle: String
	): Map<String,String> {
		return mapOf(
			"Gradle-Version" to "Gradle " + project.getGradle().getGradleVersion(),
			"Created-By" to System.getProperty("java.runtime.version"),
			"Specification-Vendor" to projectVendorName,
			"Implementation-Vendor" to projectVendorName,
			"Implementation-Vendor-Id" to projectVendorId,
			"Specification-Version" to projectSpecificationVersion,
			"Implementation-Version" to projectImplementationVersion,
			"Specification-Title" to specificationTitle,
			"Implementation-Title" to implementationTitle
	    )
	}

	fun archiveFilter(
		line: String
	): String {
		var s = line;
		s = s.replace("%IMPLEMENTATION_VERSION%", projectImplementationVersion);
		s = s.replace("provider/CRX", "provider/" + providerName);
		s = s.replace("provider=CRX", "provider=" + providerName);
		s = s.replace("jdbc_opencrx_CRX", datasourceName);
		s = s.replace("org.opencrx.core.CRX", "org.opencrx.core." + providerName);
		s = s.replace("CRX.jdbc", providerName + ".jdbc");
		s = s.replace("org/opencrx/core/CRX", "org/opencrx/core/" + providerName);
		s = s.replace("opencrx-core-CRX-App", "opencrx-core-" + providerName + "-App");
		s = s.replace("opencrx_core_CRX", "opencrx_core_" + providerName);
		s = s.replace("opencrx-core-CRX-Web", "opencrx-core-" + providerName + "-Web");
		s = s.replace("opencrx-core-CRX", "opencrx-core-" + providerName);
		s = s.replace("opencrx-ical-CRX", "opencrx-ical-" + providerName);
		s = s.replace("opencrx-caldav-CRX", "opencrx-caldav-" + providerName);
		s = s.replace("opencrx-carddav-CRX", "opencrx-carddav-" + providerName);
		s = s.replace("opencrx-webdav-CRX", "opencrx-webdav-" + providerName);
		s = s.replace("opencrx-imap-CRX", "opencrx-imap-" + providerName);
		s = s.replace("opencrx-vcard-CRX", "opencrx-vcard-" + providerName);
		s = s.replace("opencrx-spaces-CRX", "opencrx-spaces-" + providerName);
		s = s.replace("opencrx-rest-CRX", "opencrx-rest-" + providerName);
		s = s.replace("opencrx-bpi-CRX", "opencrx-bpi-" + providerName);
		s = s.replace("opencrx-calendar-CRX", "opencrx-calendar-" + providerName);
		s = s.replace("opencrx-documents-CRX", "opencrx-documents-" + providerName);
		s = s.replace("opencrx-contacts-CRX", "opencrx-contacts-" + providerName);
		s = s.replace("org.opencrx.kernel.aop2.Configuration", configurationClassName);
		s = s.replace("org.opencrx.kernel.layer.application.OpenCrxKernel_2", kernelApplicationPluginClassName);
		s = s.replace("org.opencrx.kernel.layer.model.AccessControl_2", kernelModelPluginClassName);
		s = s.replace("org.opencrx.kernel.layer.persistence.Audit_2", kernelPersistencePluginClassName);
		s = s.replace("org.opencrx.security.layer.application.OpenCrxSecurity_2", securityApplicationPluginClassName);
		s = s.replace("Provider qualifiedName=\"CRX\"", "Provider qualifiedName=\"" + providerName + "\"");
		s = s.replace("%IMPLEMENTATION_VERSION%", projectImplementationVersion);
		s = s.replace("imap:143", "imap:" + imapListenPort);
		s = s.replace("globalInterfaceLanguage='en_US'", "globalInterfaceLanguage=" + calendarGlobalInterfaceLanguage);
		s = s.replace("globalInterfaceCustomLanguages=[]", "globalInterfaceCustomLanguages=[" + calendarGlobalInterfaceCustomLanguages + "]");
		return s;
	}
	
}
