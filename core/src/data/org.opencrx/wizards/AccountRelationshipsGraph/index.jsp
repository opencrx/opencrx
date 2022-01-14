<%@  page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Description: Draw membership graph for an account
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * * Redistribution and use in source and binary forms, with or without
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
%><%@ page session="true" import="
java.util.*,
java.io.*,
java.text.*,
java.math.*,
java.net.*,
java.sql.*,
javax.naming.Context,
javax.naming.InitialContext,
org.openmdx.kernel.log.SysLog,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.exception.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.base.query.*
" %>

<%!

	private static String getNodeTitle(
		org.opencrx.kernel.account1.jmi1.Account account,
		ApplicationContext app
	) {
	  StringBuilder title = new StringBuilder();
	  boolean isDisabled = account.isDisabled() != null && account.isDisabled().booleanValue();

	  title.append(
	    isDisabled ? "<del>" : ""
	  ).append(
		  app.getHtmlEncoder().encode(new ObjectReference(account, app).getTitle(), false)
		).append(
		  isDisabled ? "</del>" : ""
		);
		return title.toString();
  }

    private static String getClickableNodeTitle(
        org.opencrx.kernel.account1.jmi1.Account account,
        ApplicationContext app,
        String requestId
    ) {
        ObjectReference accountRef = new ObjectReference(account, app);
        boolean isDisabled = account.isDisabled() != null && account.isDisabled().booleanValue();
        String test = account.refMofId();
        int plusLocation = test.indexOf("+");
        while (plusLocation >= 0) {
            test = test.substring(0, plusLocation) + "%2B" + test.substring(plusLocation + 1);
            plusLocation = test.indexOf("+");
        }
        String encodedAccountXri = test;

        StringBuilder title = new StringBuilder();
        title.append(
          "<a title='center node and reload tree' href='index.jsp?xri=" + encodedAccountXri + "&requestId=" + requestId + "'>&gt;o&lt;</a><br>" + "<a title='load in openCRX' href='../../" + accountRef.getSelectObjectAction().getEncodedHRef(requestId) + "'>*</a> " + getNodeTitle(account,app)
        );
        /*
        title.append(
            getNodeTitle(account,app)
        );
        */
        return title.toString();
    }

	private static String getJSON ( /* Tree */
		Set parents,
		org.opencrx.kernel.account1.jmi1.Account account,
		org.opencrx.kernel.account1.jmi1.AccountMembership membership,
		String relationshipKey,
		Map M,
		String indent,
		int level,
		Map memberRoleTexts,
		HttpServletResponse response,
		ApplicationContext app,
		String requestId
	) {
	  StringBuilder json = new StringBuilder();
		if(level > 0) {
			String relationships = new String();
			if (membership != null && memberRoleTexts != null) {
			  for(Iterator roles = membership.getMemberRole().iterator(); roles.hasNext(); ) {
			    if (relationships.length() > 0) {
			      relationships += ", ";
			    }
			    relationships += (String)memberRoleTexts.get(new Short(((Short)roles.next()).shortValue()));
			  }
			}

			String accountId = account.refGetPath().getLastSegment().toString();
			if (accountId.indexOf("+") > 0) {
			  System.out.println("accountId="+accountId);
			}
			json.append(
				"\n" + indent
			).append(
				"{\"id\":\"" + accountId + "\""
			).append(
				 ",\"name\":\"" + getClickableNodeTitle(account, app, requestId) + "\""
			).append(
         ",\"data\":{" + (membership == null ? "" : "\"key\":\"" + relationshipKey + "\",\"relationships\":\"" + (relationships.length() == 0 ? "-" : relationships) + "\"") + "}"
      ).append(
				",\"children\":["
			);
			Map C = (Map)M.get(account);
			if(C != null) {
				String separator = "";
				for(Iterator i = C.keySet().iterator(); i.hasNext(); ) {
					org.opencrx.kernel.account1.jmi1.Account child = (org.opencrx.kernel.account1.jmi1.Account)i.next();
					if(child != null && !parents.contains(child)) {
						Set parentsWithChild = new HashSet(parents);
						parentsWithChild.add(child);
						String jsonChild = getJSON(
							parentsWithChild,
							child,
							(org.opencrx.kernel.account1.jmi1.AccountMembership)C.get(child),
							accountId,
							M,
							indent + "  ",
							level - 1,
							memberRoleTexts,
							response,
							app,
							requestId
						);
						if(jsonChild.length() > 0) {
							json.append(
								separator
							).append(
								jsonChild
							);
							separator = ",";
						}
					}
				}
		  }
			json.append(
			  "]"
			).append(" }");
		}
		return json.toString();
	}

  private static String getJSONgraph ( /* graph */
        Set parents,
        org.opencrx.kernel.account1.jmi1.Account account,
        org.opencrx.kernel.account1.jmi1.AccountMembership membership,
        String relationshipKey,
        Map M,
        String indent,
        int level,
        Map memberRoleTexts,
        HttpServletResponse response,
        ApplicationContext app,
        String requestId
  ) {
      StringBuilder json = new StringBuilder();
        if(level > 0) {
            String relationships = new String();
            if (membership != null && memberRoleTexts != null) {
              for(Iterator roles = membership.getMemberRole().iterator(); roles.hasNext(); ) {
                if (relationships.length() > 0) {
                  relationships += ", ";
                }
                relationships += (String)memberRoleTexts.get(new Short(((Short)roles.next()).shortValue()));
              }
            }

            String accountId = account.refGetPath().getLastSegment().toString();
            if (accountId.indexOf("+") > 0) {
              System.out.println("accountId="+accountId);
            }
            json.append(
                "\n" + indent
            ).append(
                "{\"id\":\"" + accountId + "\""
            ).append(
                 ",\"name\":\"" + getClickableNodeTitle(account, app, requestId) + "\""
            ).append(
                ",\"adjacencies\":["
            );

            // add adjacencies to children
            Map C = (Map)M.get(account);
            if(C != null) {
                String separator = "";
                for(Iterator i = C.keySet().iterator(); i.hasNext(); ) {
                    org.opencrx.kernel.account1.jmi1.Account child = (org.opencrx.kernel.account1.jmi1.Account)i.next();
                    if(child != null && !parents.contains(child)) {
                    	  String childId = child.refGetPath().getLastSegment().toString();
                        json.append(
                            separator
                        ).append(
                            "{ nodeTo:\"" + childId + "\""
                        ).append(
                            ",data:{" + (membership == null ? "" : "\"rel\":\"" + (relationships.length() == 0 ? "-" : relationships) + "\"") + "}"
                        ).append(
                            "}"
                        );
                        separator = ",";
                    }
                }
            }
            json.append(
              "]"
            ).append(
              "}"
            );

            // process child nodes
            // Map C = (Map)M.get(account);
            if(C != null) {
                for(Iterator i = C.keySet().iterator(); i.hasNext(); ) {
                    org.opencrx.kernel.account1.jmi1.Account child = (org.opencrx.kernel.account1.jmi1.Account)i.next();
                    if(child != null && !parents.contains(child)) {
                        Set parentsWithChild = new HashSet(parents);
                        parentsWithChild.add(child);
                        String jsonChild = getJSON(
                            parentsWithChild,
                            child,
                            (org.opencrx.kernel.account1.jmi1.AccountMembership)C.get(child),
                            accountId,
                            M,
                            indent + "  ",
                            level - 1,
                            memberRoleTexts,
                            response,
                            app,
                            requestId
                        );
                        if(jsonChild.length() > 0) {
                            json.append(
                                ","
                            ).append(
                                jsonChild
                            );
                        }
                    }
                }
            }
        }
        return json.toString();
    }

    private static void addNode(
		org.opencrx.kernel.account1.jmi1.AccountMembership membership,
		org.opencrx.kernel.account1.jmi1.Account accountFrom,
		org.opencrx.kernel.account1.jmi1.Account accountTo,
		Map M
	) {
		Map C = (Map)M.get(accountFrom);
		if(C == null) {
			M.put(
				accountFrom,
				C = new HashMap()
			);
		}
		org.opencrx.kernel.account1.jmi1.AccountMembership N = (org.opencrx.kernel.account1.jmi1.AccountMembership)C.get(accountTo);
		if(N == null) {
			C.put(
				accountTo,
				N = membership
			);
		}
	}

	private static void addRelationships(
		org.opencrx.kernel.account1.jmi1.Account account,
		Map M,
		int level,
		javax.jdo.PersistenceManager pm
	) {
		if(level >= 0) {
			int[] maxCounts = new int[]{5, 5, 25};
			int maxCount = maxCounts[level];

 			org.opencrx.kernel.account1.cci2.AccountMembershipQuery membershipQuery = (org.opencrx.kernel.account1.cci2.AccountMembershipQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.AccountMembership.class);
 			org.openmdx.base.rest.cci.QueryExtensionRecord queryFilter = org.openmdx.base.persistence.cci.PersistenceHelper.newQueryExtension(membershipQuery);
			Collection memberships = null;
 			int count = 0;
			// acountFrom=account [ignore mebers if account is disabled!!!]
			if ((account.isDisabled() == null) || (!account.isDisabled().booleanValue())) {
	  			membershipQuery.forAllDisabled().isFalse();
	  			membershipQuery.distance().greaterThanOrEqualTo(new Integer(-1));
	  			membershipQuery.distance().lessThanOrEqualTo(new Integer(1));
	  			membershipQuery.thereExistsAccountFrom().equalTo(account);
				// HINT_DBOBJECT allows to qualify the DbObject to use.
				// For distance +/-1 memberships use ACCTMEMBERSHIP1 instead of ACCTMEMBERSHIP
		        queryFilter.setClause(
		          "(" + org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes.HINT_DBOBJECT + "1 */ (1=1) ) and " +
		          "( " +
		          "v.member IN ( " +
		          "  select distinct(member) from oocke1_tobj_acctmembership1 m, oocke1_account a " +
		          "  where " +
		          "   ((m.disabled is null) or (m.disabled = '0')) and " +
		          "   ((m.account_to   = a.object_id) and ((a.disabled is null) or (a.disabled = '0'))) " +
		          "  ) " +
		          ") "
		        );
  			memberships = account.getAccountMembership(membershipQuery);
  			for(Iterator i = memberships.iterator(); i.hasNext(); ) {
  				org.opencrx.kernel.account1.jmi1.AccountMembership membership = (org.opencrx.kernel.account1.jmi1.AccountMembership)i.next();
  				try {
	  				addNode(
	  					membership,
	  					membership.getAccountFrom(),
	  					membership.getAccountTo(),
	  					M
	  				);
  				} catch (Exception e) {
						SysLog.warning("error adding node of membership " + membership.getIdentity());
  				}
  				try {
	  				addNode(
	  					membership,
	  					membership.getAccountTo(),
	  					membership.getAccountFrom(),
	  					M
	  				);
  				} catch (Exception e) {
						SysLog.warning("error adding node of membership " + membership.getIdentity());
  				}
  				try {
	  				if (membership.getAccountTo() != null) {
	  	  				addRelationships(
	  	  					membership.getAccountTo(),
	  	  					M,
	  	  					level - 1,
	  	  					pm
	  	  				);
	  	  				count++;
    			    }
  				} catch (Exception e) {
						SysLog.warning("error adding relationship of membership " + membership.getIdentity());
  				}
  				if(count > maxCount) break;
  			}
  	  }

			// acountTo=account
			membershipQuery = (org.opencrx.kernel.account1.cci2.AccountMembershipQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.AccountMembership.class);
			membershipQuery.forAllDisabled().isFalse();
			membershipQuery.distance().greaterThanOrEqualTo(new Integer(-1));
			membershipQuery.distance().lessThanOrEqualTo(new Integer(1));
			membershipQuery.thereExistsAccountTo().equalTo(account);
			// HINT_DBOBJECT allows to qualify the DbObject to use.
			// For distance +/-1 memberships use ACCTMEMBERSHIP1 instead of ACCTMEMBERSHIP
            queryFilter.setClause(
            		org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes.HINT_DBOBJECT + "1 */ (1=1)"
            );
			memberships = account.getAccountMembership(membershipQuery);
			count = 0;
			for(Iterator i = memberships.iterator(); i.hasNext(); ) {
				org.opencrx.kernel.account1.jmi1.AccountMembership membership = (org.opencrx.kernel.account1.jmi1.AccountMembership)i.next();
				try {
					addNode(
						membership,
						membership.getAccountFrom(),
						membership.getAccountTo(),
						M
					);
 				} catch (Exception e) {
					SysLog.warning("error adding node of membership " + membership.getIdentity());
 				}
				try {
					addNode(
						membership,
						membership.getAccountTo(),
						membership.getAccountFrom(),
						M
					);
 				} catch (Exception e) {
					SysLog.warning("error adding node of membership " + membership.getIdentity());
 				}
				try {
					if (membership.getAccountFrom() != null) {
           	addRelationships(
           		membership.getAccountFrom(),
           		M,
           		level - 1,
           		pm
           	);
           	count++;
           }
 				} catch (Exception e) {
					SysLog.warning("error adding relationship of membership " + membership.getIdentity());
 				}
				if(count > maxCount) break;
			}
		}
	}

%>

<%
	request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getValue(WebKeys.APPLICATION_KEY);
	ViewsCache viewsCache = (ViewsCache)session.getValue(WebKeys.VIEW_CACHE_KEY_SHOW);
	String requestId =  request.getParameter(Action.PARAMETER_REQUEST_ID);
    String objectXri = request.getParameter("xri");
	if(app==null || objectXri==null || viewsCache.getView(requestId) == null) {
    response.sendRedirect(
       request.getContextPath() + "/" + WebKeys.SERVLET_NAME
    );
    return;
  }
  javax.jdo.PersistenceManager pm = app.getNewPmData();
	Texts_1_0 texts = app.getTexts();

	try {
		Codes codes = app.getCodes();
		short currentLocale = app.getCurrentLocaleAsIndex();

		Path objectPath = new Path(objectXri);
		String providerName = objectPath.get(2); // e.g. CRX
		String segmentName = objectPath.get(4);  // e.g. Standard

		RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));
		org.opencrx.kernel.account1.jmi1.Account account = (org.opencrx.kernel.account1.jmi1.Account)obj;

		// Derive membership matrix M for account. M has the structure M[accountFrom,accountTo] where
		// a matrix element is the distance of the relationship accountFrom->accountTo starting from account.
		Map M = new HashMap();
		addRelationships(
			account,
			M,
			2,
			pm
		);
		HashSet parents = new HashSet();
		parents.add(account);
		String json =  getJSON(
			parents,
			account,
			null, // membership
			null, // relationship key
			M,
			"",
			//Math.min(M.size(), 4),
			Math.min(Math.max(1, M.size()), 4),
			app.getCodes().getLongTextByCode(
				"memberRole",
				app.getCurrentLocaleAsIndex(),
				true
			),
			response,
			app,
			requestId
		);
		//System.out.println("JSON1=" + json);
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
	<title>Relationships Graph</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<link rel='shortcut icon' href='../../images/favicon.ico' />
	<!-- JIT -->
	<link href="../../_style/infovis.css" rel="stylesheet" type="text/css" />
	<style type="text/css" media="all">
		#infovis {
		background-color:#222;
		}
		.node {
		background-color: #222;
		color:orange;
		font-weight:bold;
		cursor:pointer;
		padding:2px;
		}
	</style>
	<!--[if IE]><script language="javascript" type="text/javascript" src="../../js/jit/excanvas.js"></script><![endif]-->
	<script language="javascript" type="text/javascript" src="../../js/jit/jit-yc.js"></script>
	<script type="text/javascript">
        var Log = {
            elem: false,
            write: function(text){
                if (!this.elem)
                    this.elem = document.getElementById('log');
                this.elem.innerHTML = text;
                //this.elem.style.left = (500 - this.elem.offsetWidth / 2) + 'px';
            }
        };

        function addEvent(obj, type, fn) {
            if (obj.addEventListener) obj.addEventListener(type, fn, false);
            else obj.attachEvent('on' + type, fn);
        };
    		function init() {
       			var json =  <%= json %>;
       			var infovis = document.getElementById('infovis');
            var w = 600;
            var h = 600;
            try {
                w = infovis.offsetWidth - 250;
                try {
                    h = window.innerHeight - 20;
                } catch (e) {}
                try {
                    h = document.body.clientHeight - 20;
                } catch (e) {}
                if (h < 200) {h = 200;}
                infovis.style.width = w + 'px';
                infovis.style.height = h + 'px';
            } catch (e) {
                alert("h=" + h);
                alert("innerHeight=" + window.innerHeight);
            }

            //init canvas
            //Create a new canvas instance.
            var canvas = new Canvas('crxcanvas', {
                'injectInto': 'infovis',
                'width': w,
                'height': h
            });
            var ht = new Hypertree(canvas, {
                //Change node and edge styles such as
                //color, width and dimensions.
                Node: {
                    dim: 1,
                    color: "#bbb"
                },

                Edge: {
                    lineWidth: 1,
                    color: "#aaa"
                },

                getRelationship: function(G,F){
                    for(var H=0;H<G.data.length;H++){
                      var I=G.data[H];
                      if(I.key==F.id){
                        return I.value
                      }
                    }
                    for(var H=0;H<F.data.length;H++){
                      var I=F.data[H];
                      if(I.key==G.id){
                        return I.value
                      }
                    }
                },

                onBeforeCompute: function(node){
                    Log.write("centering");
                },
                //Attach event handlers and add text to the
                //labels. This method is only triggered on label
                //creation
                onCreateLabel: function(domElement, node){
                    domElement.innerHTML = node.name;
                    addEvent(domElement, 'click', function () {
                        ht.onClick(node.id);
                    });
                },
                //Change node styles when labels are placed
                //or moved.
                onPlaceLabel: function(domElement, node){
                    var style = domElement.style;
                    style.display = '';
                    style.cursor = 'pointer';
                    if (node._depth <= 0) {
                        style.fontSize = "0.9em";
                        style.color = "#fff";
                    } else if(node._depth == 1){
                        style.fontSize = "0.75em";
                        style.color = "#EC8D00";
                    } else {
                        style.fontSize = "0.6em";
                        style.color = "#FFBD59";
                    }

                    var left = parseInt(style.left);
                    var w = domElement.offsetWidth;
                    style.left = (left - w / 2) + 'px';
                },

                onAfterCompute: function(){
                    Log.write("");

                    //Build the left column relationship list.
                    //This is done by collecting the information (stored in the data property)
                    //for all the nodes adjacent to the centered node.
                    var F = this;
                    var node = Graph.Util.getClosestNodeToOrigin(ht.graph, "pos");
                    var html = "<h2>" + node.name + "</h2><b>Relationships:</b>";
                    html += "<ul>";
                    Graph.Util.eachAdjacency(node, function(adj){
                        var nodeTo = adj.nodeTo;
                        if(nodeTo.data){
                        	var rel = (nodeTo.data.key == node.id) ? nodeTo.data.relationships : node.data.relationships;
                        	html += "<li>" + nodeTo.name + ' <div class=\"relation\">(relationship: ' + rel + ")</div></li>"
                        }
                    });
                    html += "</ul>";
                    document.getElementById('inner-details').innerHTML = html;
                }
            });

            //load JSON data.
            try {
                ht.loadJSON(json);
                //compute positions and plot.
                ht.refresh();
                ht.controller.onAfterCompute();

			       } catch(e) {
                document.getElementById('inner-details').innerHTML = 'root node disabled or no relationships<br>or relationships to disabled accounts only';
			       }
		    }
	</script>
</head>
<body onload="init();">

<div id="viscontainer">
    <div id="left-container">
		<div id="details" class="toggler left-item">
		  Root: <b><%= getClickableNodeTitle(account, app, requestId) %></b><br>
		  <input type="Submit" name="Cancel.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="8020" value="X" onClick="javascript:window.close();" style="float:right;margin:5px 5px 0px 0px;" /><br>
		  Node Limits: 25 - 5 - 5
		</div>
		<div class="inner" id="inner-details"></div>
    </div>

    <div id="center-container">
        <div id="infovis"></div>
    </div>

    <!--
    <div id="right-container">
    </div>
    -->

    <div id="log">calculating relationships - please wait...</div>
</div>
<%
	}
	catch (Exception e) {
 	  new ServiceException(e).log();
  } finally {
	  if(pm != null) {
		  pm.close();
	  }		  
  }
%>
</body>
</html>
