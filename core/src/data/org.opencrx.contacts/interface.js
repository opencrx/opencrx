/*
CardDavMATE - the open source CardDAV Web Client
Copyright (C) 2011-2015
    Jan Mate <jan.mate@inf-it.com>
    Andrej Lezo <andrej.lezo@inf-it.com>
    Matej Mihalik <matej.mihalik@inf-it.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.
*/

// reorder countries according to localization (returns array becaouse object are unsorted according to ECMA)
function sortCountries(obj)
{
	var arr=[];
	for(var prop in obj)
		if(obj.hasOwnProperty(prop))
			arr.push({'key': prop, 'value': obj[prop], 'translated_value': localization[globalInterfaceLanguage]['txtAddressCountry'+prop.toUpperCase()]});

	return arr.sort(function(a, b){return a.translated_value.customCompare(b.translated_value, globalSortAlphabet, 1, false)});
}

function loadAdditionalCardDAVCollections()
{
	if(globalSettingsSaving!='')
		return false;
	globalSettingsSaving='addressbook';

	var inSettings = $.extend({},globalSettings);
	var rex = new RegExp('^(https?://)([^@/]+(?:@[^@/]+)?)@(.*)');
	var key='loadedaddressbookcollections';

	inSettings.loadedaddressbookcollections = {value : new Array(), locked: globalSettings[key].locked};
	$('#ResourceCardDAVList').find('.unloadCheck').each(function(cin,cel)
	{
		if($(cel).prop('checked'))
		{
			var uidParts=$(cel).attr('data-id').match(rex);
			inSettings.loadedaddressbookcollections.value.splice(inSettings.loadedaddressbookcollections.value.length , 0, uidParts[1]+uidParts[3]);
		}
	});

	if($(inSettings[key]).not(globalSettings[key].value).length > 0 || $(globalSettings[key].value).not(inSettings[key]).length > 0)
	{
		$('#AddressbookOverlay').removeClass('loader_hidden');
		$('#ResourceCardDAVList').find('input[type="checkbox"]').prop('disabled',true);
		var setC=0;
		for(var i=0;i<globalAccountSettings.length;i++)
			if(globalAccountSettings[i].href.indexOf(globalLoginUsername)!=-1 && globalAccountSettings[i].settingsAccount)
			{
				setC++;
				netSaveSettings(globalAccountSettings[i], inSettings, false, true);
				break;
			}
		if(setC==0)
			cancelUnloadedCardDAVCollections();
	}
	else
		hideUnloadedCardDAVCollections(true);
}

function showUnloadedCardDAVCollections()
{
	if(globalAddressbookCollectionsLoading)
		return false;
	globalAddressbookCollectionsLoading=true;
	if(isAvaible('CalDavZAP'))
	{
		$('#showUnloadedCalendars').css('display','none');
		$('#showUnloadedCalendarsTODO').css('display','none');
	}
	$('#ResourceCardDAVList').find('input[type="checkbox"]').prop('disabled',true);
	$('#AddressbookOverlay').children('.loaderInfo').text(localization[globalInterfaceLanguage].loadingCollectionList).parent().fadeIn(300);
	var resList=$('#ResourceCardDAVList');
	var resHeader='.resourceCardDAV_header';
	var resItem='.resourceCardDAV';

	$('#ResourceCardDAVList').find('input[type="checkbox"]').prop('disabled',false);
	$('#AddressbookOverlay').children('.loaderInfo').text('').parent().addClass('loader_hidden');
	resList.find('.resourceCardDAV_selected').removeClass('resourceCardDAV_selected');
	resList.find('input').css('display','none');
	// header display
	resList.children('.resourceCardDAV_header').each(function(){
		if($(this).css('display')=='none')
			$(this).addClass('unloaded').css('display','');
		var headerClickElm = $('<input type="checkbox" class="unloadCheckHeader" style="position:absolute;top:3px;right:0px;margin-right:6px;"/>');
		headerClickElm.change(function(){
			loadResourceChBoxClick(this, '#ResourceCardDAVList', resHeader, resItem, '.resourceCardDAV_item');
		});
		$(this).addClass('load_mode').append(headerClickElm);
	});
	// carddav_item display
	resList.find('.resourceCardDAV').each(function(){
		if(typeof $(this).attr('data-id') != 'undefined')
		{
			var newInputElm = $('<input type="checkbox" class="unloadCheck" data-id="'+$(this).attr('data-id')+'" style="position:absolute;top:8px;right:0px;margin-right:6px;"/>');
			newInputElm.change(function(){
				loadCollectionChBoxClick(this, '#ResourceCardDAVList', resHeader, resItem, '.resourceCardDAV_item');
			});
			$(this).siblings('.contact_group').addBack().addClass('load_mode');
			$(this).append(newInputElm);
			if($(this).parent().css('display')=='none')
				$(this).addClass('unloaded');
			else
				newInputElm.prop('checked',true);
			newInputElm.trigger('change');
		}
	});
	$('#showUnloadedAddressbooks').css('display','none');
	$('.resourcesCardDAV_h').text(localization[globalInterfaceLanguage].txtEnabledAddressbooks);
	var origH = resList.find('.resourceCardDAV_header.unloaded').eq(0).css('height');
	var origC = resList.find('.resourceCardDAV.unloaded').parent().eq(0).css('height');
	resList.find('.resourceCardDAV_header.unloaded').css({'height':0,'display':''}).animate({height:origH},300);
	resList.find('.resourceCardDAV.unloaded').parent().css({'height':0,'display':''}).animate({height:origC},300);
	resList.animate({'top':49},300);
}

function cancelUnloadedCardDAVCollections()
{
	$('#ResourceCardDAVList').children('.resourceCardDAV_item').children('.resourceCardDAV ').each(function(){
		var uidParts=$(this).attr('data-id').match(RegExp('^(https?://)([^@/]+(?:@[^@/]+)?)@(.*)'));
		var checkHref=uidParts[1]+uidParts[3];
		var isLoaded=false;
		if(typeof globalCrossServerSettingsURL!='undefined'&&globalCrossServerSettingsURL!=null&globalCrossServerSettingsURL)
		{
			var uidParts=$(this).attr('data-id').match(RegExp('/([^/]+/[^/]+/)$'));
			var tmpParts = uidParts[1].match('^(.*/)([^/]+)/$');
			var checkHref=decodeURIComponent(tmpParts[1])+tmpParts[2]+'/';
			var found=false;
			for(var l=0;l<globalSettings.loadedaddressbookcollections.value.length;l++)
			{
				var tmpParts2 = globalSettings.loadedaddressbookcollections.value[l].match('^(.*/)([^/]+)/([^/]+)/$');
				var checkHref2=decodeURIComponent(tmpParts2[2])+'/'+tmpParts2[3]+'/';
				if(checkHref==checkHref2)
				{
					found=true;
					break;
				}
			}
			isLoaded=found;
		}
		else
			isLoaded=(globalSettings.loadedaddressbookcollections.value.indexOf(checkHref)!=-1)
		var unloadCh=$(this).find('.unloadCheck');
		var checked=unloadCh.prop('checked');

		if((isLoaded && !checked) || (!isLoaded && checked))
			unloadCh.prop('checked',!checked).trigger('change');
	});
	hideUnloadedCardDAVCollections(true);
}

function hideUnloadedCardDAVCollections(withCallback)
{
	var resList=$('#ResourceCardDAVList');
	resList.find(':input.unloadCheck').remove();
	resList.find(':input.unloadCheckHeader').remove();
	resList.find('.load_mode').removeClass('load_mode');
	resList.find(':input').css('display','');
	resList.find('.resourceCardDAV').not('.unloaded').parent().css('height','');

	$('.resourcesCardDAV_h').text(localization[globalInterfaceLanguage].txtAddressbooks);
	resList.find('.resourceCardDAV_header.unloaded').animate({height:0},300).promise().done(function(){
		resList.find('.resourceCardDAV_header.unloaded').css({'display':'none','height':''});
	});
	resList.find('.resourceCardDAV.unloaded').parent().animate({height:0},300).promise().done(function(){
		resList.find('.resourceCardDAV.unloaded').parent().css({'display':'none','height':''});
		resList.find('.resourceCardDAV_header').not('.unloaded').each(function(){
			var triggerInput=$(this).nextUntil('.resourceCardDAV_header').filter(':visible').first().find('input[type="checkbox"]');
			collectionChBoxClick(triggerInput.get(0), '#ResourceCardDAVList', '.resourceCardDAV_header', '.resourceCardDAV', null, false);
		});
		resList.find('.unloaded').removeClass('unloaded');
		globalAddressbookCollectionsLoading=false;
		if(withCallback)
			hideUnloadedCardDAVCollectionsCallBack();
	});
	resList.animate({'top':24},300);
	if(withCallback)
		$('#AddressbookOverlay').fadeOut(300,function(){
			$(this).removeClass('loader_hidden');
		});
	if(isAvaible('CalDavZAP'))
	{
		$('#showUnloadedCalendars').css('display','block');
		$('#showUnloadedCalendarsTODO').css('display','block');
	}
}

function hideUnloadedCardDAVCollectionsCallBack()
{
	if(globalAddressbookList.contactLoaded!=null)
		globalAddressbookList.loadContactByUID(globalAddressbookList.contactLoaded.uid);
	$('#showUnloadedAddressbooks').css('display','');
	globalFirstHideLoader=true;
	globalSettingsSaving='';
	selectActiveAddressbook();
	$('#AddressbookOverlay').css('display','none');
	$('#ResourceCardDAVList').find('input[type="checkbox"]').prop('disabled',false);
}

function selectActiveAddressbook()
{
	if(globalAddressbookCollectionsLoading)
		return false;

	for(var i=0; i<globalResourceCardDAVList.collections.length;i++)
		if(globalResourceCardDAVList.collections[i].uid!=undefined)
		{
			var inputResource=globalResourceCardDAVList.collections[i].uid;
			var par=inputResource.split('/');
			if(globalSettings.addressbookselected.value!='')
			{
				if(typeof globalSettings.addressbookselected.value=='string' && inputResource==globalSettings.addressbookselected.value.substring(0,globalSettings.addressbookselected.value.lastIndexOf('/')+1))
				{
					if($('#ResourceCardDAVList').find('.resourceCardDAV_selected:visible').length == 0)
						$('#ResourceCardDAVList').find('.resourceCardDAV[data-id="'+globalSettings.addressbookselected.value+'"]:visible').addClass('resourceCardDAV_selected');
				}
				else if(typeof globalSettings.addressbookselected.value=='string' && globalSettings.addressbookselected.value.charAt(globalSettings.addressbookselected.value.length-1)=='/' && (par[par.length-3]+'/'+par[par.length-2]+'/')==globalSettings.addressbookselected.value)
				{
					if($('#ResourceCardDAVList').find('.resourceCardDAV_selected:visible').length == 0)
						$('#ResourceCardDAVList').find('.resourceCardDAV[data-id="'+inputResource+'"]:visible').addClass('resourceCardDAV_selected');
				}
				else if(typeof globalSettings.addressbookselected.value=='string' && globalSettings.addressbookselected.value.charAt(globalSettings.addressbookselected.value.length-1)!='/')
				{
					if((par[par.length-3]+'/'+par[par.length-2]+'/') == globalSettings.addressbookselected.value.substring(0,globalSettings.addressbookselected.value.lastIndexOf('/')+1) && $('#ResourceCardDAVList').find('.resourceCardDAV_selected:visible').length == 0)
					{
						if($('#ResourceCardDAVList').find('.resourceCardDAV[data-id="'+inputResource+globalSettings.addressbookselected.value.substring(globalSettings.addressbookselected.value.lastIndexOf('/')+1,globalSettings.addressbookselected.value.length)+'"]:visible').length>0)
						$('#ResourceCardDAVList').find('.resourceCardDAV[data-id="'+inputResource+globalSettings.addressbookselected.value.substring(globalSettings.addressbookselected.value.lastIndexOf('/')+1,globalSettings.addressbookselected.value.length)+'"]:visible').addClass('resourceCardDAV_selected');
					}
				}
				else if (typeof globalSettings.addressbookselected.value=='object' && inputResource.match(globalSettings.addressbookselected.value)!=null)
				{
					if($('#ResourceCardDAVList').find('.resourceCardDAV_selected:visible').length == 0)
						$('#ResourceCardDAVList').find('.resourceCardDAV[data-id="'+inputResource+'"]:visible').addClass('resourceCardDAV_selected');
				}
			}
		}

	if($('#ResourceCardDAVList').find('.resourceCardDAV_selected:visible').length==0)
		for(var i=0; i<globalResourceCardDAVList.collections.length;i++)
			if(globalResourceCardDAVList.collections[i].uid!=undefined)
			{
				var inputResource=globalResourceCardDAVList.collections[i].uid;
				var par=inputResource.split('/');
				if(typeof globalAddressbookSelected!='undefined' && globalAddressbookSelected!=null && globalAddressbookSelected!='')
				{
					globalSettings.addressbookselected.value = globalAddressbookSelected;
					if(typeof globalAddressbookSelected=='string' && inputResource==globalAddressbookSelected.substring(0,globalAddressbookSelected.lastIndexOf('/')+1))
					{
						if($('#ResourceCardDAVList').find('.resourceCardDAV_selected:visible').length == 0)
							$('#ResourceCardDAVList').find('[data-id="'+globalAddressbookSelected+'"]:visible').addClass('resourceCardDAV_selected');
					}
					else if(typeof globalAddressbookSelected=='string' && globalAddressbookSelected.charAt(globalAddressbookSelected.length-1)=='/' && (par[par.length-3]+'/'+par[par.length-2]+'/')==globalAddressbookSelected)
					{
						if($('#ResourceCardDAVList').find('.resourceCardDAV_selected:visible').length == 0)
							$('#ResourceCardDAVList').find('[data-id="'+inputResource+'"]:visible').addClass('resourceCardDAV_selected');
					}
					else if(typeof globalAddressbookSelected=='string' && globalAddressbookSelected.charAt(globalAddressbookSelected.length-1)!='/')
					{
						if((par[par.length-3]+'/'+par[par.length-2]+'/') == globalAddressbookSelected.substring(0,globalAddressbookSelected.lastIndexOf('/')+1) && $('#ResourceCardDAVList').find('.resourceCardDAV_selected:visible').length == 0)
						{
							if($('#ResourceCardDAVList').find('[data-id="'+inputResource+globalAddressbookSelected.substring(globalAddressbookSelected.lastIndexOf('/')+1,globalAddressbookSelected.length)+'"]:visible').length>0)
							$('#ResourceCardDAVList').find('[data-id="'+inputResource+globalAddressbookSelected.substring(globalAddressbookSelected.lastIndexOf('/')+1,globalAddressbookSelected.length)+'"]:visible').addClass('resourceCardDAV_selected');
						}
					}
					else if (typeof globalAddressbookSelected=='object' && inputResource.match(globalAddressbookSelected)!=null)
					{
						if($('#ResourceCardDAVList').find('.resourceCardDAV_selected:visible').length == 0)
							$('#ResourceCardDAVList').find('[data-id="'+inputResource+'"]:visible').addClass('resourceCardDAV_selected');
					}
					else if($('#ResourceCardDAVList').find('.resourceCardDAV_selected:visible').length == 0)
					{
						globalSettings.addressbookselected.value=par[par.length-3]+'/'+par[par.length-2]+'/';
						$('#ResourceCardDAVList').find('[data-id="'+inputResource+'"]:visible').addClass('resourceCardDAV_selected');
					}
				}
			}

	if($('#ResourceCardDAVList').find('.resourceCardDAV_selected:visible').length == 0 && $('#ResourceCardDAVList').find('.resourceCardDAV[data-id]:visible').length > 0)
	{
		var ui_d = $('#ResourceCardDAVList').find('.resourceCardDAV[data-id]:visible').eq(0).attr('data-id');
		var part_u = ui_d.split('/');
		globalSettings.addressbookselected.value=part_u[part_u.length-3]+'/'+part_u[part_u.length-2]+'/';
		$('#ResourceCardDAVList').find('.resourceCardDAV[data-id]:visible').eq(0).addClass('resourceCardDAV_selected');
	}
	var selColl=globalResourceCardDAVList.getCollectionByUID($('#ResourceCardDAVList').find('.resourceCardDAV[data-id].resourceCardDAV_selected').attr('data-id'));
	if(selColl!=null)
	{
		selColl.filterUID=selColl.uid;
		if(selColl.permissions.read_only==true)
			globalRefAddContact.addClass('element_no_display');
		else
			globalRefAddContact.removeClass('element_no_display');

		globalRefAddContact.attr('data-url', selColl.uid.replace(RegExp('[^/]+$'),''));
		globalRefAddContact.attr('data-filter-url',selColl.filterUID);	// Set the current addressbook filter uid
		globalRefAddContact.attr('data-account-uid',selColl.accountUID);
		globalRefAddContact.attr('data-color',selColl.color);

		// Make the selected collection active
		if(!globalCardDAVInitLoad)
		{
			if(typeof(globalContactsABChange)=='function')
				globalContactsABChange(selColl.uid);

			$('#ResourceCardDAVList').find('.resourceCardDAV_item').find('.resourceCardDAV_selected').removeClass('resourceCardDAV_selected');
			$('#ResourceCardDAVList').find('[data-id='+jqueryEscapeSelector(selColl.uid)+']').addClass('resourceCardDAV_selected');
			if(selColl.filterUID[selColl.filterUID.length-1]!='/')
				$('#ResourceCardDAVList').find('[data-id='+jqueryEscapeSelector(selColl.filterUID)+']').addClass('resourceCardDAV_selected');
		}
	}
}

function CardDAVUpdateMainLoader(inputCollection)
{
	if(!globalCardDAVInitLoad)
	{
		if(globalSettingsSaving=='addressbook')
		{
			globalLoadedCollectionsCount++;
			$('#AddressbookOverlay').children('.loaderInfo').text(localization[globalInterfaceLanguage].loadingAddressbooks.replace('%act%', globalLoadedCollectionsCount).replace('%total%', globalLoadedCollectionsNumber));
			if(globalSettingsSaving!='' && globalLoadedCollectionsCount==globalLoadedCollectionsNumber)
				setTimeout(function(){hideUnloadedCardDAVCollectionsCallBack();if(isAvaible('Settings'))hideSettingsOverlay();},300);
		}
		selectActiveAddressbook();
		for(var adr in globalAddressbookList.vcard_groups)
		{
			if(globalAddressbookList.vcard_groups[adr].length>0)
			{
				extendDestSelect();
				if(typeof $('#vCardEditor').attr('data-vcard-uid')=='undefined')
					$('#vCardEditor').find('[data-attr-name="_DEST_"]').find('optiotn[data-type$="'+$('#ResourceCardDAVList').find('.resourceCardDAV_selected').find(':input[data-id]').attr('data-id')+'"]').prop('selected',true)
			}
		}
		return false;
	}
	if(inputCollection.makeLoaded)
	{
		globalAddressbookNumberCount++;
		$('#MainLoaderInner').html(localization[globalInterfaceLanguage].loadingAddressbooks.replace('%act%', (globalAddressbookNumberCount)).replace('%total%', globalAddressbookNumber));
	}

	inputCollection.isLoaded=true;
	$('#ResourceCardDAVList [data-id="'+inputCollection.uid+'"]').removeClass('r_operate');

	var unloadedCount=0;
	for(var i=0; i<globalResourceCardDAVList.collections.length;i++)
		if(globalResourceCardDAVList.collections[i].uid!=undefined && !globalResourceCardDAVList.collections[i].isLoaded)
				unloadedCount++;

	if(unloadedCount==0 && !isCardDAVLoaded)
	{
		globalCardDAVInitLoad=false;
		globalAddressbookList.renderContacs();
		var rexo=new RegExp('^(https?://)([^@/]+(?:@[^@/]+)?)@(.*)');
		if(!globalDefaultAddressbookCollectionActiveAll)
		{
			for(var i=0;i<globalSettings.activeaddressbookcollections.value.length;i++)
			{
				if(typeof globalCrossServerSettingsURL!='undefined'&&globalCrossServerSettingsURL!=null&globalCrossServerSettingsURL)
				{
					var tmpParts2 = globalSettings.activeaddressbookcollections.value[i].match('^(.*/)([^/]+)/([^/]+)/$');
					var checkHref2=tmpParts2[2]+'/'+tmpParts2[3]+'/';
					if($('#ResourceCardDAVList input[data-id$="'+checkHref2+'"]:visible').length>0)
						$('#ResourceCardDAVList input[data-id$="'+checkHref2+'"]').trigger('click');
				}
				else
				{
					var uidPart=globalSettings.activeaddressbookcollections.value[i].match(RegExp('^(https?://)(.*)', 'i'))[1];
					var uidPart2=globalSettings.activeaddressbookcollections.value[i].match(RegExp('^(https?://)(.*)', 'i'))[2];
					var uidPart3=getAccount(inputCollection.accountUID).userAuth.userName;
					var uid = uidPart+uidPart3+'@'+uidPart2;
					if($('#ResourceCardDAVList .resourceCardDAV input[data-id="'+uid+'"]:visible').length>0)
						$('#ResourceCardDAVList .resourceCardDAV input[data-id="'+uid+'"]').trigger('click');
				}
			}
			if(globalSettings.activeaddressbookcollections.value.length>0 && $('#ResourceCardDAVList .resourceCardDAV input:checked').length==0)
				globalDefaultAddressbookCollectionActiveAll=true;
		}
		if(globalDefaultAddressbookCollectionActiveAll)
			for(var i=0;i<globalResourceCardDAVList.collections.length;i++)
				if(globalResourceCardDAVList.collections[i].uid!=undefined && $('#ResourceCardDAVList .resourceCardDAV input[data-id="'+globalResourceCardDAVList.collections[i].uid+'"]:visible').length>0)
						$('#ResourceCardDAVList input[data-id="'+globalResourceCardDAVList.collections[i].uid+'"]').trigger('click');
		selectActiveAddressbook();
		globalRefAddContact.prop('disabled', false);
		loadNextApplication(true);
	}
}

function applyAddrSettings(abContactRef, remValues)
{
	var addrVals = new Array();
	if(typeof remValues!= 'undefined')
	{
		abContactRef.find('[data-type="\\%address"]').find('[data-type="country_type"]').each(function(){
		addrVals[$(this).parents('[data-type="%address"]').attr('data-id')] = $(this).val();
		});
	}
	var country_option=abContactRef.find('[data-type="\\%address"]').find('[data-type="country_type"]').find('option').last().clone();

	abContactRef.find('[data-type="\\%address"]').find('[data-type="country_type"]').html('');

	// we need a copy of the object because of the next "delete" operation
	var addressTypesTmp=jQuery.extend({}, addressTypes);

	// delete custom ordered element before the sort (then we will add them back)
	if(globalSettings.addresscountryfavorites.value.length>0)
		for(var i=globalSettings.addresscountryfavorites.value.length-1;i>=0;i--)
			delete addressTypesTmp[globalSettings.addresscountryfavorites.value[i]];

	var addressTypesArr=sortCountries(addressTypesTmp);
	// re-add custom ordered elements from the original addressTypes (where all elements are still present)
	if(globalSettings.addresscountryfavorites.value.length>0)
		for(var i=globalSettings.addresscountryfavorites.value.length-1;i>=0;i--)
			addressTypesArr.unshift({'key': globalSettings.addresscountryfavorites.value[i], 'value': addressTypes[globalSettings.addresscountryfavorites.value[i]], 'translated_value': localization[globalInterfaceLanguage]['txtAddressCountry'+globalSettings.addresscountryfavorites.value[i].toUpperCase()]});

	for(var i=0;i<addressTypesArr.length;i++)
	{
		var tmp=country_option;
		tmp.attr('data-type',addressTypesArr[i].key);
		tmp.attr('data-full-name',addressTypesArr[i].value[0]);
		tmp.text(addressTypesArr[i].translated_value);	// translation
		abContactRef.find('[data-type="\\%address"]').find('[data-type="country_type"]').append(tmp.clone());
	}
	abContactRef.find('[data-type="\\%address"]').find('[data-type="country_type"]').attr('data-autoselect',globalSettings.defaultaddresscountry.value);
	for(var key in addrVals)
		abContactRef.find('[data-type="\\%address"][data-id="'+key+'"]').find('[data-type="country_type"]').val(addrVals[key]);
}

function localizeCardDAV()
{
	// frequently used
	var abContactRef=$('#ABContact');

	// restore original templates
	$('#ResourceCardDAVList').empty().append(globalOrigCardDAVListTemplate.clone());
	abContactRef.empty().append(globalOrigVcardTemplate.clone());


	localizeAddressTypes();

	// interface translation
	$('[data-type="system_logo"]').attr('alt',localization[globalInterfaceLanguage].altLogo);
	$('[data-type="system_username"]').attr('placeholder',localization[globalInterfaceLanguage].pholderUsername);
	$('[data-type="system_password"]').attr('placeholder',localization[globalInterfaceLanguage].pholderPassword);

	$('[data-type="resourcesCardDAV_txt"]').text(localization[globalInterfaceLanguage].txtAddressbooks);
	$('[data-type="contact_txt"]').text(localization[globalInterfaceLanguage].txtContact);
	$('[data-type="search"]').attr('placeholder',localization[globalInterfaceLanguage].txtSearch);
	$('#AddContact').attr('alt',localization[globalInterfaceLanguage].altAddContact);
	$('#AddContact').attr('title',localization[globalInterfaceLanguage].altAddContact);
	$('#Logout').attr('alt',localization[globalInterfaceLanguage].altLogout);
	$('#Logout').attr('title',localization[globalInterfaceLanguage].altLogout);
	$('#showUnloadedAddressbooks').attr({title:capitalize(localization[globalInterfaceLanguage].txtEnabledAddressbooks),alt:capitalize(localization[globalInterfaceLanguage].txtEnabledAddressbooks)});
	$('#loadUnloadedAddressbooks').val(localization[globalInterfaceLanguage].buttonSave);
	$('#loadUnloadedAddressbooksCancel').val(localization[globalInterfaceLanguage].buttonCancel);

	abContactRef.find('#photoBox').find('h1').text(localization[globalInterfaceLanguage].txtRemoteImage);
	abContactRef.find('#photoURL').attr('placeholder',localization[globalInterfaceLanguage].pholderUrlVal);

	abContactRef.find('[data-type="photo"]').text(localization[globalInterfaceLanguage].altPhoto);
	abContactRef.find('[data-type="given"]').attr('placeholder',localization[globalInterfaceLanguage].pholderGiven);
	abContactRef.find('[data-type="family"]').attr('placeholder',localization[globalInterfaceLanguage].pholderFamily);
	abContactRef.find('[data-type="middle"]').attr('placeholder',localization[globalInterfaceLanguage].pholderMiddle);
	abContactRef.find('[data-type="nickname"]').attr('placeholder',localization[globalInterfaceLanguage].pholderNickname);
	abContactRef.find('[data-type="ph_firstname"]').attr('placeholder',localization[globalInterfaceLanguage].pholderPhGiven);
	abContactRef.find('[data-type="ph_lastname"]').attr('placeholder',localization[globalInterfaceLanguage].pholderPhFamily);
	abContactRef.find('[data-type="prefix"]').attr('placeholder',localization[globalInterfaceLanguage].pholderPrefix);
	abContactRef.find('[data-type="suffix"]').attr('placeholder',localization[globalInterfaceLanguage].pholderSuffix);
	abContactRef.find('[data-type="date_bday"]').attr('placeholder',localization[globalInterfaceLanguage].pholderBday);
	abContactRef.find('[data-type="title"]').attr('placeholder',localization[globalInterfaceLanguage].pholderTitle);
	abContactRef.find('[data-type="org"]').attr('placeholder',localization[globalInterfaceLanguage].pholderOrg);
	abContactRef.find('[data-type="department"]').attr('placeholder',localization[globalInterfaceLanguage].pholderDepartment);
	abContactRef.find('span[data-type="company_contact"]').text(localization[globalInterfaceLanguage].txtCompanyContact);

	abContactRef.find('[data-type="\\%del"]').attr('alt',localization[globalInterfaceLanguage].altDel);
	abContactRef.find('[data-type="\\%add"]').attr('alt',localization[globalInterfaceLanguage].altAdd);
	abContactRef.find('[data-type="value_handler"]').attr('alt',localization[globalInterfaceLanguage].altValueHandler);

	abContactRef.find('[data-type=":custom"]').text(localization[globalInterfaceLanguage].txtCustom);
	abContactRef.find('[data-type="custom_value"]').attr('placeholder',localization[globalInterfaceLanguage].pholderCustomVal);

	abContactRef.find('[data-type="dates_txt"]').text(localization[globalInterfaceLanguage].txtDates);
	abContactRef.find('[data-type="\\%date"]').find('input[data-type="date_value"]').attr('placeholder',localization[globalInterfaceLanguage].pholderDate);
	abContactRef.find('[data-type="\\%date"]').find('[data-type=":_$!<anniversary>!$_:"]').text(localization[globalInterfaceLanguage].txtDatesAnniversary);
	abContactRef.find('[data-type="\\%date"]').find('[data-type=":_$!<other>!$_:"]').text(localization[globalInterfaceLanguage].txtDatesOther);

	abContactRef.find('[data-type="phone_txt"]').text(localization[globalInterfaceLanguage].txtPhone);
	abContactRef.find('[data-type="\\%phone"]').find('input[data-type="value"]').attr('placeholder',localization[globalInterfaceLanguage].pholderPhoneVal);
	abContactRef.find('[data-type="\\%phone"]').find('[data-type="work"]').text(localization[globalInterfaceLanguage].txtPhoneWork);
	abContactRef.find('[data-type="\\%phone"]').find('[data-type="home"]').text(localization[globalInterfaceLanguage].txtPhoneHome);
	abContactRef.find('[data-type="\\%phone"]').find('[data-type="cell"]').text(localization[globalInterfaceLanguage].txtPhoneCell);
	abContactRef.find('[data-type="\\%phone"]').find('[data-type="cell,work"]').text(localization[globalInterfaceLanguage].txtPhoneCellWork);
	abContactRef.find('[data-type="\\%phone"]').find('[data-type="cell,home"]').text(localization[globalInterfaceLanguage].txtPhoneCellHome);
	abContactRef.find('[data-type="\\%phone"]').find('[data-type="main"]').text(localization[globalInterfaceLanguage].txtPhoneMain);
	abContactRef.find('[data-type="\\%phone"]').find('[data-type="pager"]').text(localization[globalInterfaceLanguage].txtPhonePager);
	abContactRef.find('[data-type="\\%phone"]').find('[data-type="fax"]').text(localization[globalInterfaceLanguage].txtPhoneFax);
	abContactRef.find('[data-type="\\%phone"]').find('[data-type="fax,work"]').text(localization[globalInterfaceLanguage].txtPhoneFaxWork);
	abContactRef.find('[data-type="\\%phone"]').find('[data-type="fax,home"]').text(localization[globalInterfaceLanguage].txtPhoneFaxHome);
	abContactRef.find('[data-type="\\%phone"]').find('[data-type="iphone"]').text(localization[globalInterfaceLanguage].txtPhoneIphone);
	abContactRef.find('[data-type="\\%phone"]').find('[data-type="other"]').text(localization[globalInterfaceLanguage].txtPhoneOther);

	abContactRef.find('[data-type="email_txt"]').text(localization[globalInterfaceLanguage].txtEmail);
	abContactRef.find('[data-type="\\%email"]').find('input[data-type="value"]').attr('placeholder',localization[globalInterfaceLanguage].pholderEmailVal);
	abContactRef.find('[data-type="\\%email"]').find('[data-type="internet,work"]').text(localization[globalInterfaceLanguage].txtEmailWork);
	abContactRef.find('[data-type="\\%email"]').find('[data-type="home,internet"]').text(localization[globalInterfaceLanguage].txtEmailHome);
	abContactRef.find('[data-type="\\%email"]').find('[data-type=":mobileme:,internet"]').text(localization[globalInterfaceLanguage].txtEmailMobileme);
	abContactRef.find('[data-type="\\%email"]').find('[data-type=":_$!<other>!$_:,internet"]').text(localization[globalInterfaceLanguage].txtEmailOther);

	abContactRef.find('[data-type="url_txt"]').text(localization[globalInterfaceLanguage].txtUrl);
	abContactRef.find('[data-type="\\%url"]').find('input[data-type="value"]').attr('placeholder',localization[globalInterfaceLanguage].pholderUrlVal);
	abContactRef.find('[data-type="\\%url"]').find('[data-type="work"]').text(localization[globalInterfaceLanguage].txtUrlWork);
	abContactRef.find('[data-type="\\%url"]').find('[data-type="home"]').text(localization[globalInterfaceLanguage].txtUrlHome);
	abContactRef.find('[data-type="\\%url"]').find('[data-type=":_$!<homepage>!$_:"]').text(localization[globalInterfaceLanguage].txtUrlHomepage);
	abContactRef.find('[data-type="\\%url"]').find('[data-type=":_$!<other>!$_:"]').text(localization[globalInterfaceLanguage].txtUrlOther);

	abContactRef.find('[data-type="related_txt"]').text(localization[globalInterfaceLanguage].txtRelated);
	abContactRef.find('[data-type="\\%person"]').find('input[data-type="value"]').attr('placeholder',localization[globalInterfaceLanguage].pholderRelatedVal);
	abContactRef.find('[data-type="\\%person"]').find('[data-type=":_$!<manager>!$_:"]').text(localization[globalInterfaceLanguage].txtRelatedManager);
	abContactRef.find('[data-type="\\%person"]').find('[data-type=":_$!<assistant>!$_:"]').text(localization[globalInterfaceLanguage].txtRelatedAssistant);
	abContactRef.find('[data-type="\\%person"]').find('[data-type=":_$!<father>!$_:"]').text(localization[globalInterfaceLanguage].txtRelatedFather);
	abContactRef.find('[data-type="\\%person"]').find('[data-type=":_$!<mother>!$_:"]').text(localization[globalInterfaceLanguage].txtRelatedMother);
	abContactRef.find('[data-type="\\%person"]').find('[data-type=":_$!<parent>!$_:"]').text(localization[globalInterfaceLanguage].txtRelatedParent);
	abContactRef.find('[data-type="\\%person"]').find('[data-type=":_$!<brother>!$_:"]').text(localization[globalInterfaceLanguage].txtRelatedBrother);
	abContactRef.find('[data-type="\\%person"]').find('[data-type=":_$!<sister>!$_:"]').text(localization[globalInterfaceLanguage].txtRelatedSister);
	abContactRef.find('[data-type="\\%person"]').find('[data-type=":_$!<child>!$_:"]').text(localization[globalInterfaceLanguage].txtRelatedChild);
	abContactRef.find('[data-type="\\%person"]').find('[data-type=":_$!<friend>!$_:"]').text(localization[globalInterfaceLanguage].txtRelatedFriend);
	abContactRef.find('[data-type="\\%person"]').find('[data-type=":_$!<spouse>!$_:"]').text(localization[globalInterfaceLanguage].txtRelatedSpouse);
	abContactRef.find('[data-type="\\%person"]').find('[data-type=":_$!<partner>!$_:"]').text(localization[globalInterfaceLanguage].txtRelatedPartner);
	abContactRef.find('[data-type="\\%person"]').find('[data-type=":_$!<other>!$_:"]').text(localization[globalInterfaceLanguage].txtRelatedOther);

	abContactRef.find('[data-type="im_txt"]').text(localization[globalInterfaceLanguage].txtIm);
	abContactRef.find('[data-type="\\%im"]').find('input[data-type="value"]').attr('placeholder',localization[globalInterfaceLanguage].pholderImVal);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="work"]').text(localization[globalInterfaceLanguage].txtImWork);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="home"]').text(localization[globalInterfaceLanguage].txtImHome);
	abContactRef.find('[data-type="\\%im"]').find('[data-type=":mobileme:"]').text(localization[globalInterfaceLanguage].txtImMobileme);
	abContactRef.find('[data-type="\\%im"]').find('[data-type=":_$!<other>!$_:"]').text(localization[globalInterfaceLanguage].txtImOther);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="aim"]').text(localization[globalInterfaceLanguage].txtImProtAim);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="icq"]').text(localization[globalInterfaceLanguage].txtImProtIcq);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="irc"]').text(localization[globalInterfaceLanguage].txtImProtIrc);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="jabber"]').text(localization[globalInterfaceLanguage].txtImProtJabber);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="msn"]').text(localization[globalInterfaceLanguage].txtImProtMsn);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="yahoo"]').text(localization[globalInterfaceLanguage].txtImProtYahoo);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="facebook"]').text(localization[globalInterfaceLanguage].txtImProtFacebook);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="gadugadu"]').text(localization[globalInterfaceLanguage].txtImProtGadugadu);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="googletalk"]').text(localization[globalInterfaceLanguage].txtImProtGoogletalk);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="qq"]').text(localization[globalInterfaceLanguage].txtImProtQq);
	abContactRef.find('[data-type="\\%im"]').find('[data-type="skype"]').text(localization[globalInterfaceLanguage].txtImProtSkype);

	abContactRef.find('[data-type="profile_txt"]').text(localization[globalInterfaceLanguage].txtProfile);
	abContactRef.find('[data-type="\\%profile"]').find('input[data-type="value"]').attr('placeholder',localization[globalInterfaceLanguage].pholderProfileVal);
	abContactRef.find('[data-type="\\%profile"]').find('[data-type="twitter"]').text(localization[globalInterfaceLanguage].txtProfileTwitter);
	abContactRef.find('[data-type="\\%profile"]').find('[data-type="facebook"]').text(localization[globalInterfaceLanguage].txtProfileFacebook);
	abContactRef.find('[data-type="\\%profile"]').find('[data-type="flickr"]').text(localization[globalInterfaceLanguage].txtProfileFlickr);
	abContactRef.find('[data-type="\\%profile"]').find('[data-type="linkedin"]').text(localization[globalInterfaceLanguage].txtProfileLinkedin);
	abContactRef.find('[data-type="\\%profile"]').find('[data-type="myspace"]').text(localization[globalInterfaceLanguage].txtProfileMyspace);
	abContactRef.find('[data-type="\\%profile"]').find('[data-type="sinaweibo"]').text(localization[globalInterfaceLanguage].txtProfileSinaweibo);

	abContactRef.find('[data-type="address_txt"]').text(localization[globalInterfaceLanguage].txtAddress);
	abContactRef.find('[data-type="\\%address"]').find('[data-type="work"]').text(localization[globalInterfaceLanguage].txtAddressWork);
	abContactRef.find('[data-type="\\%address"]').find('[data-type="home"]').text(localization[globalInterfaceLanguage].txtAddressHome);
	abContactRef.find('[data-type="\\%address"]').find('[data-type=":_$!<other>!$_:"]').text(localization[globalInterfaceLanguage].txtAddressOther);

	abContactRef.find('[data-type="categories_txt"]').text(localization[globalInterfaceLanguage].txtCategories);

	abContactRef.find('[data-type="note_txt"]').text(localization[globalInterfaceLanguage].txtNote);
	abContactRef.find('[data-type="\\%note"]').find('textarea[data-type="value"]').attr('placeholder',localization[globalInterfaceLanguage].pholderNoteVal);

	abContactRef.find('[data-type="edit"]').val(localization[globalInterfaceLanguage].buttonEdit);
	abContactRef.find('[data-type="add_contact"]').val(localization[globalInterfaceLanguage].altAddContact);
	abContactRef.find('[data-type="save"]').val(localization[globalInterfaceLanguage].buttonSave);
	abContactRef.find('[data-type="cancel"]').val(localization[globalInterfaceLanguage].buttonCancel);
	abContactRef.find('[data-type="delete_from_group"]').val(localization[globalInterfaceLanguage].buttonDeleteFromGroup);
	abContactRef.find('[data-type="delete"]').val(localization[globalInterfaceLanguage].buttonDelete);

	// hook for extension specific localization
	if(typeof(globalContactsExtLocalize)=='function')
		globalContactsExtLocalize(abContactRef);

	globalTranslCardDAVListTemplate=$('#ResourceCardDAVListTemplate').clone();
	globalTranslCardDAVListHeader=globalTranslCardDAVListTemplate.find('.resourceCardDAV_header').clone();
	globalTranslCardDAVListItem=globalTranslCardDAVListTemplate.find('.resourceCardDAV_item').clone();

	globalTranslVcardTemplate=$('#vCardTemplate').contents().clone();

	// CUSTOM PLACEHOLDER (initialization for the whole page)
	$('input[placeholder],textarea[placeholder]').placeholder();
}

function processEditorElements(inputEditorRef, processingType, inputIsReadonly, inputIsCompany)
{
	var cssShowAsTxtClass='element_show_as_text';
	var cssGrayedTxt='element_grayed';
	var cssElementNoDisplay='element_no_display';
	var cssElementHide='element_hide';

	var tmp_ref=inputEditorRef;

	if(processingType=='hide')
	{
		tmp_ref.attr('data-editor-state', 'show');
		var disabled=true;
		var readonly=true;
	}
	else
	{
		tmp_ref.attr('data-editor-state', 'edit');
		var disabled=false;
		var readonly=false;
	}

	var inputLockedElements=[];
	if(typeof(globalContactsExtGetLockedElements)=='function')
		inputLockedElements=globalContactsExtGetLockedElements(inputIsCompany);

	var inputDisabledElements=[];
	if(typeof(globalContactsExtGetDisabledElements)=='function')
		inputDisabledElements=globalContactsExtGetDisabledElements(inputIsCompany);

	// show "drag" border on photo & delete button
	tmp_ref.find('#photo_drag').css('display', (disabled || readonly ? 'none' : 'inline'));
	// if the editor state is "edit" show the "delete" button
	if(!tmp_ref.find('#photo').hasClass('photo_blank'))
		tmp_ref.find('#reset_img').css('display', (disabled || readonly ? 'none': 'inline'));

	// checkboxes
	var tmp=tmp_ref.find('[type="checkbox"]');
	tmp.each(
		function(index,element)
		{
			var tmp=$(element);
			var tmp_data_type=tmp.attr('data-type');
			tmp.prop('disabled', disabled || inputLockedElements.indexOf('[data-type="'+tmp_data_type+'"]')!=-1);
			if(!tmp.prop('checked') && (processingType=='hide' || inputLockedElements.indexOf('[data-type="'+tmp_data_type+'"]')!=-1))
				tmp.parent().addClass(cssGrayedTxt);
			else
				tmp.parent().removeClass(cssGrayedTxt);
		}
	);

	tmp_ref.find('input[data-type^="date_"]').prop('disabled', disabled || readonly);

	// family name, given name, and organization name
	var typeList=['family', 'given', 'middle', 'nickname', 'prefix', 'suffix', 'ph_firstname', 'ph_lastname', 'date_bday', 'tags', 'title', 'department', 'org'];
	for(var i=0; i<typeList.length; i++)
	{
		var elementRef = tmp_ref.find('[data-type="'+typeList[i]+'"]');
		var elementDisabled = inputDisabledElements.indexOf('[data-type="'+typeList[i]+'"]')!=-1 && (processingType=='add' || elementRef.val()=='');
		var elementReadOnly = readonly || inputLockedElements.indexOf('[data-type="'+typeList[i]+'"]')!=-1 || elementDisabled;

		elementRef.prop({'readonly':elementReadOnly, 'disabled':elementDisabled}).toggleClass('non_editable', elementDisabled);
	}

	var tmp_tags_prefix=['#tags'];
	var typeList=new Array({sel: '[data-type="\\%address"]'}, {sel: '[data-type="\\%phone"]'}, {sel: '[data-type="\\%email"]'}, {sel: '[data-type="\\%url"]'}, {sel: '[data-type="\\%date"]'}, {sel: '[data-type="\\%person"]'}, {sel: '[data-type="\\%im"]'}, {sel: '[data-type="\\%profile"]'}, {sel: '[data-type="\\%categories"]'}, {sel: '[data-type="\\%note"]'});
	// hook for extending the list of editor elements
	if(typeof(globalContactsExtGetAdditionalElements)=='function')
	{
		tmp_tags_prefix=tmp_tags_prefix.concat(globalContactsExtGetAdditionalElements('tags'));
		typeList=typeList.concat(globalContactsExtGetAdditionalElements('common'));
	}

	tmp_ref.find('select').prop('disabled', disabled);
	/*************************** BAD HACKS SECTION ***************************/
	if($.browser.msie || $.browser.mozilla)
	{
		var newSVG=$((disabled ? SVG_select_dis : SVG_select)).attr('data-type', 'select_icon').css({'pointer-events': 'none', 'z-index': '1', 'display': 'inline', 'margin-left': (disabled ? '-22px' : '-19px'), 'vertical-align': 'top', 'background-color': '#ffffff'});	// background-color = stupid IE9 bug

		//XXXX check this - was $('#ABContact')
		tmp_ref.find('select').parent().find('svg[data-type="select_icon"]').replaceWith($('<div>').append($(newSVG).clone()).html());
	}
	/*************************** END OF BAD HACKS SECTION ***************************/

	for(var i=0; i<tmp_tags_prefix.length; i++)
	{
		var tmp=tmp_ref.find(tmp_tags_prefix[i]+'_tag');
		tmp.prop('readonly', readonly);
		if(readonly)
			tmp.closest('div.tagsinput').addClass('readonly');
		else
			tmp.closest('div.tagsinput').removeClass('readonly');
	}

	for(var i=0; i<typeList.length; i++)
	{
		var found_non_empty=0;
		var empty = false;
		tmp=tmp_ref.find(typeList[i].sel);

		tmp.each(
			function(index, element)
			{
				var tmp=$(element).find('[data-type="value"]');
				if(tmp.length==0)
					tmp=$(element).find('[data-type="date_value"]');
				var found=0;
				// check if there is any data present (if not, whe hide the element)
				if($(element).attr('data-type')=='%address')	// address is handled specially
					tmp.each(
						function(index,element)
						{
							if($(element).attr('data-addr-field')!='' && $(element).attr('data-addr-field')!='country' && $(element).val()!='')
							{
								found=1;
								return false;
							}
						}
					);
				else if(tmp.val()!='')	// other elements (not address)
					found=1;

				if(processingType=='hide')
				{
					if(found)
					{
						$(element).find('[data-type="\\%add"]').find('input[type="image"]').addClass(cssElementNoDisplay);
						$(element).find('[data-type="\\%del"]').find('input[type="image"]').addClass(cssElementNoDisplay);
						$(element).find('select').prop('disabled', disabled);
						$(element).find('textarea').prop('disabled', disabled);
						/*************************** BAD HACKS SECTION ***************************/
						if($.browser.msie || $.browser.mozilla)
						{
							var newSVG=$((disabled ? SVG_select_dis : SVG_select)).attr('data-type', 'select_icon').css({'pointer-events': 'none', 'z-index': '1', 'display': 'inline', 'margin-left': (disabled ? '-22px' : '-19px'), 'vertical-align': 'top', 'background-color': '#ffffff'});	// background-color = stupid IE9 bug

							//XXXX check this - was $('#ABContact')
							$(element).find('svg[data-type="select_icon"]').replaceWith($('<div>').append($(newSVG).clone()).html());
						}
						/*************************** END OF BAD HACKS SECTION ***************************/
						tmp.prop('readonly', readonly);
						found_non_empty=1;
					}
					else
						$(element).addClass(cssElementNoDisplay);
				}
				else	// 'show'
				{
					empty = empty || $(element).hasClass(cssElementNoDisplay);

					$(element).removeClass(cssElementNoDisplay);
					$(element).find('[data-type="\\%add"]').find('input[type="image"]').removeClass(cssElementNoDisplay);
					$(element).find('[data-type="\\%del"]').find('input[type="image"]').removeClass(cssElementNoDisplay);
					$(element).find('select').prop('disabled', disabled);
					$(element).find('textarea').prop('disabled', disabled);
					/*************************** BAD HACKS SECTION ***************************/
					if($.browser.msie || $.browser.mozilla)
					{
						var newSVG=$((disabled ? SVG_select_dis : SVG_select)).attr('data-type', 'select_icon').css({'pointer-events': 'none', 'z-index': '1', 'display': 'inline', 'margin-left': (disabled ? '-22px' : '-19px'), 'vertical-align': 'top', 'background-color': '#ffffff'});	// background-color = stupid IE9 bug
						//XXXX check this - was $('#ABContact')
						$(element).find('svg[data-type="select_icon"]').replaceWith($('<div>').append($(newSVG).clone()).html());
					}
					/*************************** END OF BAD HACKS SECTION ***************************/
					tmp.prop('readonly', readonly);
				}
			}
		);

		if(processingType==='show' && !empty) {
			if(typeList[i].sel==='[data-type="\\%address"]') {
				tmp.each(function() {
					var street = $(this).find('[data-addr-field="street"]');
					if(street.val()) {
						street.trigger('keyup.street');
					}
				});
			}

			if(typeof globalContactAutoExpand=='undefined' || globalContactAutoExpand!=false)
				tmp.last().find('[data-type="\\%add"]').find('.op').trigger('click');
		}

		// set the visibility of the buttons
		if(processingType=='hide')
		{
			if(inputIsReadonly!=true)
			{
				if(typeof globalGroupContactsByCompanies!='undefined' && globalGroupContactsByCompanies==true && inputIsCompany)
					tmp_ref.find('[data-type="add_contact"]').removeClass(cssElementNoDisplay);
				else
					tmp_ref.find('[data-type="add_contact"]').addClass(cssElementNoDisplay);

				tmp_ref.find('[data-type="edit"]').removeClass(cssElementNoDisplay);
			}
			else
			{
				tmp_ref.find('[data-type="add_contact"]').addClass(cssElementNoDisplay);
				tmp_ref.find('[data-type="edit"]').addClass(cssElementNoDisplay);
			}

			tmp_ref.find('[data-type="save"]').addClass(cssElementNoDisplay);
			tmp_ref.find('[data-type="cancel"]').addClass(cssElementNoDisplay);
			tmp_ref.find('[data-type="delete_from_group"]').addClass(cssElementNoDisplay);
			tmp_ref.find('[data-type="delete"]').addClass(cssElementNoDisplay);
		}
		else if(processingType=='add')
		{
			tmp_ref.find('[data-type="add_contact"]').addClass(cssElementNoDisplay);
			tmp_ref.find('[data-type="edit"]').addClass(cssElementNoDisplay);
			tmp_ref.find('[data-type="save"]').removeClass(cssElementNoDisplay);
			tmp_ref.find('[data-type="cancel"]').removeClass(cssElementNoDisplay);
			tmp_ref.find('[data-type="delete_from_group"]').addClass(cssElementNoDisplay);
			tmp_ref.find('[data-type="delete"]').addClass(cssElementNoDisplay);
		}
		else
		{
			tmp_ref.find('[data-type="add_contact"]').addClass(cssElementNoDisplay);
			tmp_ref.find('[data-type="edit"]').addClass(cssElementNoDisplay);
			tmp_ref.find('[data-type="save"]').removeClass(cssElementNoDisplay);
			tmp_ref.find('[data-type="cancel"]').removeClass(cssElementNoDisplay);
			// show "Delete from Group" only if there is an active contact group

	// XXX we need to use another identificator
	//		if(globalResourceCardDAVList.getLoadedAddressbook().filterUID[globalResourceCardDAVList.getLoadedAddressbook().filterUID.length-1]!='/')
	//			tmp.find('[data-type="delete_from_group"]').removeClass(cssElementNoDisplay);

			tmp_ref.find('[data-type="delete"]').removeClass(cssElementNoDisplay);
		}

		if(!found_non_empty)
		{
			if(processingType=='hide')
				tmp.prev().addClass(cssElementNoDisplay);
			else
				tmp.prev().removeClass(cssElementNoDisplay);
		}
	}

	// set editor "process" hook
	if(typeof(globalContactsExtEditorProcess)=='function')
		globalContactsExtEditorProcess(tmp_ref, 'post', processingType, inputIsCompany);
}

function loadImage(image)
{
	var canvas = $('#photo');
	var canvasElement = canvas.get(0);
	var imageWidth = image.width;
	var imageHeight = image.height;
	var canvasWidth = canvas.width()*globalContactPhotoScaleFactor;
	var canvasHeight = canvas.height()*globalContactPhotoScaleFactor;
	var clipStartX = 0;
	var clipStartY = 0;
	var clipWidth = imageWidth;
	var clipHeight = imageHeight;

	canvasElement.width = canvasWidth;
	canvasElement.height = canvasHeight;

	if(imageWidth-canvasWidth < imageHeight-canvasHeight) {
		var clipLength = Math.ceil((imageHeight-imageWidth/canvasWidth*canvasHeight)/2);
		clipStartY = clipLength;
		clipHeight = imageHeight-clipLength*2;
	}
	else {
		var clipLength = Math.ceil((imageWidth-imageHeight/canvasHeight*canvasWidth)/2);
		clipStartX = clipLength;
		clipWidth = imageWidth-clipLength*2;
	}

	canvasElement.getContext('2d').drawImage(image, clipStartX, clipStartY, clipWidth, clipHeight, 0, 0, canvasWidth, canvasHeight);
	canvas.removeClass('photo_blank');
}

function CardDAVeditor_cleanup(inputLoadEmpty, inputIsCompany)
{
	CardDAVcleanupRegexEnvironment();

	// Cleanup the editor and store reference to the editor object
	globalRefVcardEditor=globalTranslVcardTemplate.clone();
	// cleanup old data form address fields
	globalAddressElementOldData={};

	if(typeof(globalContactsExtEditorProcess)=='function')
		globalContactsExtEditorProcess(globalRefVcardEditor, 'pre', null, inputIsCompany);

	/*************************** BAD HACKS SECTION ***************************/
	/* IE or FF */
	if($.browser.msie || $.browser.mozilla)
	{
		// ADD empty SVG to interface (we will replace it later)
		$('<svg data-type="select_icon"></svg>').css('display', 'none').insertAfter(globalRefVcardEditor.find('select[data-type$="_type"]'));

		if($.browser.msie && parseInt($.browser.version, 10)==10) /* IE 10 (because there are no more conditional comments) */
			globalRefVcardEditor.find('[data-type="\\%note"]').find('textarea[data-type="value"]').text('').attr('placeholder',$('[data-type="\\%note"]').find('textarea[data-type="value"]').attr('placeholder'));
	}
	/*************************** END OF BAD HACKS SECTION ***************************/

	// bind events (see also add_elements())
	// hide the "-" button (we maybe change this in future)
	globalRefVcardEditor.find('[data-type="\\%del"]').css('visibility', 'hidden');

	var tmp_arr=['[data-type="\\%phone"]', '[data-type="\\%email"]', '[data-type="\\%url"]', '[data-type="\\%date"]', '[data-type="\\%person"]', '[data-type="\\%im"]', '[data-type="\\%profile"]', '[data-type="\\%address"]'];
	for(var i=0; i<tmp_arr.length; i++)
	{
		globalABEditorCounter[tmp_arr[i]]=1;	// restart id counters for editor objects
		globalRefVcardEditor.find(tmp_arr[i]+' [data-type="\\%add"] input').data('customSelector', tmp_arr[i]).click(function(){add_element($(this).parent(), $(this).data('customSelector'), $(this).data('customSelector'), '[data-type="\\%add"]','[data-type="\\%del"]', globalABEditorCounter[$(this).data('customSelector')]++);checkContactFormScrollBar();});
		globalRefVcardEditor.find(tmp_arr[i]+' [data-type="\\%del"] input').data('customSelector', tmp_arr[i]).click(function(){del_element($(this).parent(), $(this).data('customSelector'), '[data-type="\\%add"]','[data-type="\\%del"]');checkContactFormScrollBar();});
		if(typeof globalContactAutoExpand=='undefined' || globalContactAutoExpand!=false)
		{
			globalRefVcardEditor.find(tmp_arr[i]+' input[type="text"]').bind('keyup', function() {
				var el = $(this);
				var row = el.closest('tr[data-type^="%"]');
				var isLast = row.attr('data-type')!==row.next().attr('data-type');

				if(isLast && el.val()) {
					row.find('[data-type="\\%add"] input').trigger('click');
				}
			});
		}
		//globalRefVcardEditor.find(tmp_arr[i]).children().filter('[data-type="\\%add"]').click();
	}
	// one special thing for address
	globalRefVcardEditor.find('[data-type="\\%address"] [data-type="country_type"]').change(function(){set_address_country(this);checkContactFormScrollBar();});

	var tmp=globalRefVcardEditor.find('[data-type="\\%address"]');
	var tmp_select=tmp.find('[data-type="country_type"]').attr('data-autoselect');
	if(tmp_select!='')
	{
		tmp.find('[data-type="country_type"]').children('[data-type="'+jqueryEscapeSelector(tmp_select)+'"]').prop('selected', true);
		tmp.find('[data-autoselect]').change();
	}

	globalRefVcardEditor.find('[data-type="custom_value"]').bind('keyup change', function(){
		$(this).parent().find('[data-type="invalid"]').css('display', (vCard.pre['custom_type'].test($(this).val()) ? 'none' : 'inline'));
	});

	// init image uploader
	globalRefVcardEditor.find('.photo_div').bind('dragover dragenter', function(event){
		event.stopPropagation();
		event.preventDefault();

		// allow image manipulation only if the editor is in "edit" state
		if($('#vCardEditor').attr('data-editor-state')!="edit")
			return false;

		event.originalEvent.dataTransfer.dropEffect='copy'; // explicitly show this is a copy
	});

	globalRefVcardEditor.find('.photo_div').bind('drop', function(event) {
		process_image(event);
	});

	globalRefVcardEditor.find('#upload_file').bind('change', function(event) {
		process_image(event);
	});

	globalRefVcardEditor.find('#photoBoxButton').bind('click', function(event) {
		var photo = $('#photoURL').val();
		var newImg = new Image();
		newImg.src = photo;
		newImg.onload = function() {
			// show the image "delete" button
			$('#reset_img').css('display', 'inline');
			// remove the template related to previous image (start with clean one)
			vCard.tplM['contentline_PHOTO'][0]=null;

			$('#photoURLHidden').val($('#photoURL').val());

			loadImage(this);
			hidePhotoBox();
		};
		newImg.onerror = function() {
			$('#photoURL').addClass('invalid');
			$('#photoBoxContent').find('[data-type="invalid"]').css('display', 'inline');
		};
	});

	// initialize tagsinput
	globalRefVcardEditor.find('#tags').tagsInput({
		'height': null,
		'width': '530px',
		'color': '#2d2d2d',
		'placeholderColor': '#e0e0e0',
		'useNativePlaceholder': true,
		'defaultText': localization[globalInterfaceLanguage].addCategory,
		'delimiter': ',',
		'allowDelimiterInValue': true,	// if true delimiter is escaped with '\' ('\' is escaped as '\\')
		'trimInput': false,
		'autocomplete_url': globalAddressbookList.getABCategories(true),
		'autocomplete': {
			'autoFocus': true,
			'minLength': 0
		},
		'onChange' : function(tag, tagImported)
		{
			// copy the array
			var xList=globalAddressbookList.getABCategories(true);
			var currentTags=$(this).val().splitCustom(',');
			for(var i=xList.length-1; i>=0; i--)
			{
				for(var j=0; j<currentTags.length; j++)
					if(xList[i] == currentTags[j])
						xList.splice(i, 1);
			}
			$('#tags_tag').autocomplete('option', 'source', xList);

			checkContactFormScrollBar();
		}
	});

	// initialize datepicker
	globalRefVcardEditor.find('input[data-type^="date_"]').focus(function(){initDatePicker($(this));});

	/*************************** BAD HACKS SECTION ***************************/
	if($.browser.msie && parseInt($.browser.version, 10)==10)	/* IE 10 (because there are no more conditional comments) */
		globalRefVcardEditor.find('#tags_tag').css({'padding-top': '1px', 'padding-left': '1px'});
	/*************************** END OF BAD HACKS SECTION ***************************/

	globalRefVcardEditor.find('[data-type="org"]').autocomplete({'source': function(request, response){var matcher=RegExp($.ui.autocomplete.escapeRegex(request.term), 'i'); response($.grep(globalAddressbookList.getABCompanies(true), function(value){value=value.label || value.value || value; return matcher.test(value) || matcher.test(value.multiReplace(globalSearchTransformAlphabet));}));}, 'minLength': 0, 'change': function(){$('[data-type="department"]').autocomplete({'source': function(request, response){var matcher=RegExp($.ui.autocomplete.escapeRegex(request.term), 'i'); response($.grep(globalAddressbookList.getABCompanyDepartments($('#vCardEditor').find('[data-type="org"]').val()), function(value){value=value.label || value.value || value; return matcher.test(value) || matcher.test(value.multiReplace(globalSearchTransformAlphabet));}));}, 'minLength': 0})}});

/*
	globalABListTop=globalRefABList.offset().top;
	globalABListLeft=globalRefABList.offset().left;

	// rewrite it and use:
	// var start=document.elementFromPoint(globalABListLeft, globalABListTop);
	globalLastScrollPos=0;	// move to the main.js

	globalRefABList.scroll(function(e){
		globalRefABListTable.children('.ablist_header:visible').each(function(index, element){
			var headerWidth=$(element).outerWidth();
			var headerHeight=$(element).outerHeight();
			var floating_elem=$('#SystemCardDavMATE > .ablist_header');

			if(globalLastScrollPos<=globalRefABList.scrollTop())	// scrolling DOWN
			{
				var next_h=$(element).nextAll('.ablist_header:visible').first();	// next visible header
				if(next_h!=null && next_h.offset().top>globalABListTop)	// only if it is below to #ABList do action
				{
					var cloned=$(element).clone();
					// do not create the floating header with the same text twice
					if(floating_elem.filter(':contains("'+jqueryEscapeSelector(cloned.text())+'")').length==0)
					{
						// parameters for the fixed element
						cloned.css({'top': globalABListTop, 'left': globalABListLeft, 'width': headerWidth, 'position': 'fixed', 'z-index': 1});
						// remove the previous floating header
						floating_elem.remove();

						// set the opacity back to standard value (item is invisible scrolled above the ABlist top)
						globalRefABListTable.children('.ablist_header').each(function(index,element){
							if($(element).css('opacity')=='0'){$(element).css('opacity',0.85);}
						});

						// set the element opacity to 0 and "replace" it with floating element above it
						$(element).css('opacity',0);
						cloned.appendTo('#SystemCardDavMATE');
					}
					// move the previous floating header UP
					if(next_h.offset().top<globalABListTop+headerHeight)	// if next header offset is immediately below to top offset
						floating_elem.css('top',globalABListTop-(globalABListTop+headerHeight-next_h.offset().top));

					return false;
				}
			}
			else	// scrolling UP
			{
				if($(element).offset().top>=globalABListTop)
				{
					var prev_h=$(element).prevAll('.ablist_header').first();
					if(prev_h!=null)	// if there is a previous header in #ABList do action
					{
						var cloned=$(prev_h).clone();
						// do not create the floating header with the same text twice
						if(floating_elem.filter(':contains("'+jqueryEscapeSelector(cloned.text())+'")').length==0)
						{
							// parameters for the fixed element
//							cloned.css('top',globalABListTop-headerHeight);
							cloned.css({'top': Math.min(globalABListTop,$(element).offset().top-headerHeight), 'left': globalABListLeft, 'width': headerWidth, 'position': 'fixed', 'z-index': 1});

							// remove the previous floating header
							floating_elem.remove();

							// set the opacity back to standard value (item is invisible scrolled above the ABlist top)
							globalRefABListTable.children('.ablist_header').each(function(index,element){
								if($(element).css('opacity')=='0'){$(element).css('opacity',0.85);}
							});

							// set the previous element opacity to 0 and "replace" it with floating element above it
							$(prev_h).css('opacity',0);
							cloned.appendTo('#SystemCardDavMATE');
						}
					}
					// move the next floating header DOWN
					if(floating_elem.length!=0 && floating_elem.offset().top<globalABListTop)
						floating_elem.css('top',Math.min(globalABListTop,$(element).offset().top-headerHeight));

					return false;
				}
			}
		});

		globalLastScrollPos=globalRefABList.scrollTop();
	});
*/

	// CUSTOM PLACEHOLDER (initialization for the editor)
	globalRefVcardEditor.find('input[placeholder],textarea[placeholder]').placeholder();
	// enable autosize for textarea elements
	globalRefVcardEditor.find('textarea[data-type="value"]').autosize({defaultStyles: {height: '64', overflow: '', 'overflow-y': '', 'word-wrap': '', resize: 'none'}, callback: function(){checkContactFormScrollBar();}});

	if(inputLoadEmpty==true)
		$('#EditorBox').fadeTo(0, 1);	/* 0 = no animation */

	return globalRefVcardEditor;
}


function animate_message(messageSelector, messageTextSelector, duration, operation)
{
	if(operation==undefined)
		operation='+=';
	var height=$(messageTextSelector).height()+14;
	var animation=400;

	$(messageSelector).animate({'max-height': height+'px', height: (operation==undefined ? '+=' : operation)+height+'px'}, animation, function(){
		if(operation=='+=')
		{
			if(messageSelector=='#ABInMessageEditBox')
			{

				$(messageTextSelector).text(localization[globalInterfaceLanguage][globalDisableAnimationMessageHiding]);
				globalObjectLoading=false
				globalDisableAnimationMessageHiding='';
			}
			else
			setTimeout(function(){
					animate_message(messageSelector, messageTextSelector, 0, '-=');
				}, duration);

		}
	});

	return duration+2*animation;
}

function show_editor_message(inputPosition, inputSetClass, inputMessage, inputDuration)
{
	if(inputPosition==undefined || inputPosition=='in')
	{
		$('#ABContact').scrollTop(0);
		messageSelector='#ABInMessage';
		messageTextSelector='#ABInMessageText';
	}
	else
	{
		messageSelector='#ABMessage';
		messageTextSelector='#ABMessageText';
	}

	$(messageTextSelector).attr('class',inputSetClass);
	$(messageTextSelector).text(inputMessage);
	return animate_message(messageSelector, messageTextSelector, inputDuration);
}

function set_address_country(inputSelectedAddressObj)
{
	var selectedCountry=$(inputSelectedAddressObj).find('option').filter(':selected').attr('data-type');
	var addressElement=$(inputSelectedAddressObj).closest('[data-type="\\%address"]');

	// store the previous data + cleanup the data-addr-fields, placeholders and values
	globalAddressElementOldData = {};

	addressElement.find('[data-addr-fid]').each(
		function(index, element)
		{
			var tmp=$(element).find('input');
			var tmp_field_name=tmp.attr('data-addr-field');

			if(tmp_field_name!=undefined && tmp_field_name!='') {
				if(!globalAddressElementOldData.hasOwnProperty(tmp_field_name)) {
					globalAddressElementOldData[tmp_field_name] = [];
				}

				globalAddressElementOldData[tmp_field_name].push({'value': tmp.val(), 'data-match': tmp.attr('data-match')});
			}

			if(tmp_field_name==='street') {
				tmp.unbind('keyup.street');
			}

			tmp.attr({'data-addr-field': '', 'data-match': '', 'placeholder': ''}).unplaceholder();	// REMOVE CUSTOM PLACEHOLDER
			tmp.val('');

			// set address country "cleanup" hook
			if(typeof(globalContactsExtAddrElemAfterCleanup)=='function')
				globalContactsExtAddrElemAfterCleanup(element);
		}
	);

	addressElement.find('[data-group="street"]').closest('tr[data-type="container"]').not(':first').remove();
	addressElement.find('[data-group]').removeAttr('data-group');

	if(addressTypes[selectedCountry]!=undefined)
		for(var i=1;i<addressTypes[selectedCountry].length;i++)
		{
			if(addressTypes[selectedCountry][i]['type']=='input')
			{
				var tmp=addressElement.find('[data-addr-fid="'+jqueryEscapeSelector(addressTypes[selectedCountry][i]['fid'])+'"]').find('input');
				tmp.attr('data-addr-field',addressTypes[selectedCountry][i]['data-addr-field']);
				tmp.attr('placeholder',addressTypes[selectedCountry][i]['placeholder']);

				if(addressTypes[selectedCountry][i]['data-addr-field']==='street') {
					tmp.closest('tr[data-type="container"]').attr('data-group', 'street');

					tmp.bind('keyup.street', function() {
						var el = $(this);
						var row = el.closest('tr[data-type="container"]');
						var isLast = row.attr('data-group')!==row.next().attr('data-group');

						if(isLast && el.val()) {
							row.clone(true).insertAfter(row).find('input').val('');
						}
					});
				}

				// here we restore the data from globalAddressElementOldData variable
				if(globalAddressElementOldData.hasOwnProperty(addressTypes[selectedCountry][i]['data-addr-field'])) {
					for(var j=0; j<globalAddressElementOldData[addressTypes[selectedCountry][i]['data-addr-field']].length; j++) {
						tmp = addressElement.find('[data-addr-fid="'+jqueryEscapeSelector(addressTypes[selectedCountry][i]['fid'])+'"]').find('input').last();
						tmp.val(globalAddressElementOldData[addressTypes[selectedCountry][i]['data-addr-field']][j]['value']);
						tmp.trigger('keyup.street');
					};
				}
			}
			else if(addressTypes[selectedCountry][i]['type']=='country')
			{
				var tmp=addressElement.find('[data-type="\\%country_container"]');
				tmp.find('select').find('option[data-type]').prop('selected', false);
				tmp.find('select').find('option[data-type="'+jqueryEscapeSelector(selectedCountry)+'"]').prop('selected', true);

				// the country selector is in wrong container -> we need to move it
				if(addressTypes[selectedCountry][i]['fid']!=tmp.closest('[data-addr-fid]').attr('data-addr-fid'))
					$(addressElement).find('[data-addr-fid="'+jqueryEscapeSelector(addressTypes[selectedCountry][i]['fid'])+'"]').append(tmp);
			}

			// set address country "update" hook
			if(typeof(globalContactsExtAddrElemAfterUpdate)=='function')
				globalContactsExtAddrElemAfterUpdate(addressElement, addressTypes[selectedCountry][i]);
		}

	// hide the unused fields by changing the CSS
	addressElement.find('[data-type="container"]').each(
		function(index,element)
		{
			var found=0;
			$(element).find('[data-addr-field]').each(
				function(index,element)
				{
					if($(element).attr('data-addr-field')!='')
					{
						found=1;
						return false;
					}
				}
			);

			if(found)
				$(element).removeClass('element_no_display_af');
			else
				$(element).addClass('element_no_display_af');
		}
	);

	// CUSTOM PLACEHOLDER (reinitialization due to possible placeholder value change)
	addressElement.find('input[data-type="value"][placeholder],textarea[data-type="value"][placeholder]').placeholder();
}

function add_element(inputElementID, inputParentSelector, newElementSelector, inputAddClassSelector, inputDelClassSelector, newElementID) // note: newElementSelector is always used with .last()
{
	// we assume that the new element is inputElementID.parent() to minimize then number of selectors!
	var newElement=inputElementID.parent().clone().wrap('<div>');	// wrap('<div>') is used because we use .find() which not searches the "self"
	// disable the "add" button on the current element (do not move above)
	inputElementID.filter(inputAddClassSelector).css('visibility', 'hidden');

	// CUSTOM PLACEHOLDER
	// remove the "placeholder" data (custom placeholder label for IE)
	newElement.find('label').remove();
	newElement.find('[data-type="date_value"],[data-type="value"]').removeAttr('id', '').removeClass('placeholder-input');

	// unselect each selected element
	newElement.find('option').prop('selected', false);
	// remove the form values
	newElement.find('[data-type$="value"], [data-type$="date_value"]').val('');
	// hide custom types
	newElement.find('[data-type="custom_span"]').css('display', 'none');
	// get the current data-id value
	var prevID=newElement.attr("data-id");
	// add the new data-id value
	newElement.attr("data-id", newElementID);

	// add element "before insert" hook
	if(typeof(globalContactsExtAddElemBeforeInsert)=='function')
		globalContactsExtAddElemBeforeInsert(newElement);

	// add the new element (with enabled "add" button) + store the reference to the current element
	var tmpRef=inputElementID.parent().after(newElement);
	// enable the "del" button on this and the previous element
	tmpRef.next().addBack().find(inputDelClassSelector).css('visibility', '');

	// now we need a reference to the new element
	var tmpRef=tmpRef.next();
	// CUSTOM PLACEHOLDER
	// enable custom placeholder support (it is enabled only if needed)
	tmpRef.find('input[data-type="value"][placeholder], input[data-type="date_value"][placeholder],textarea[data-type="value"][placeholder]').placeholder();

	// enable autosize for textarea elements
	tmpRef.find('textarea[data-type="value"]').autosize({defaultStyles: {height: '64', overflow: '', 'overflow-y': '', 'word-wrap': '', resize: 'none'}, callback: function(){checkContactFormScrollBar();}});

	//bind datepicker
	if(tmpRef.find('input[data-type="date_value"]').hasClass('hasDatepicker'))
		tmpRef.find('input[data-type="date_value"]').removeClass('hasDatepicker');
	if(tmpRef.find('input[data-type="date_value"]').parent().find('img').css('display')!='none')
		tmpRef.find('input[data-type="date_value"]').parent().find('img').css('display','none')
	tmpRef.find('input[data-type="date_value"]').focus(function(){initDatePicker($(this));});

	// bind events
	var tmp_arr=['[data-type="\\%phone"]', '[data-type="\\%email"]', '[data-type="\\%url"]', '[data-type="\\%date"]', '[data-type="\\%person"]', '[data-type="\\%im"]', '[data-type="\\%profile"]', '[data-type="\\%address"]'];
	if(tmp_arr.indexOf(inputParentSelector)!=-1)
	{

		tmpRef.find('[data-type="\\%add"] input').data('customSelector', inputParentSelector).click(function(){add_element($(this).parent(), $(this).data('customSelector'), $(this).data('customSelector'), '[data-type="\\%add"]','[data-type="\\%del"]', globalABEditorCounter[$(this).data('customSelector')]++);checkContactFormScrollBar();});
		tmpRef.find('[data-type="\\%del"] input').data('customSelector', inputParentSelector).click(function(){del_element($(this).parent(), $(this).data('customSelector'), '[data-type="\\%add"]','[data-type="\\%del"]');checkContactFormScrollBar();});
		if(typeof globalContactAutoExpand=='undefined' || globalContactAutoExpand!=false)
		{
			tmpRef.find('input[type="text"]').bind('keyup', function() {
				var el = $(this);
				var row = el.closest('tr[data-type^="%"]');
				var isLast = row.attr('data-type')!==row.next().attr('data-type');

				if(isLast && el.val()) {
					row.find('[data-type="\\%add"] input').trigger('click');
				}
			});
		}
		// one special thing for address
		if(inputParentSelector=='[data-type="\\%address"]' && tmpRef.attr('data-type')=='%address')
			tmpRef.find('[data-type="country_type"]').change(function(){set_address_country(this);checkContactFormScrollBar();});
	}

	if(inputParentSelector=='[data-type="\\%address"]')
	{
		// execute the "autoselect"
		var tmp=inputElementID.closest(inputParentSelector).next();
		var tmp_select=tmp.find('[data-autoselect]').attr('data-autoselect');
		if(tmp_select!=null)
		{
			tmp.find('[data-type="country_type"]').children('[data-type="'+jqueryEscapeSelector(tmp_select)+'"]').prop('selected', true);
			tmp.find('[data-autoselect]').change();
		}
	}

	tmpRef.find('[data-type="custom_value"]').bind('keyup change', function(){
		$(this).parent().find('[data-type="invalid"]').css('display', (vCard.pre['custom_type'].test($(this).val()) ? 'none' : 'inline'));
	});

	if(typeof(globalContactsExtAddElemAfterInsert)=='function')
		globalContactsExtAddElemAfterInsert(tmpRef, inputDelClassSelector, prevID);

	return true;
}

function del_element(inputElementID, inputParentSelector, inputAddClassSelector, inputDelClassSelector)
{
	// all elements except the last can be removed
	if(inputElementID.closest(inputParentSelector).siblings(inputParentSelector).length>0)
	{
		inputElementID.closest(inputParentSelector).remove();
		// enable the "add" button on last element
		$(inputParentSelector).last().find(inputAddClassSelector).css('visibility', '');
		// hide the "del" button if only one element is present (we maybe change this in future)
		if($(inputParentSelector).length==1)
			$(inputParentSelector).last().find(inputDelClassSelector).css('visibility', 'hidden');
	}
	else	// currently not used because the "-" button is hidden on the last element (we maybe change this in future)
		inputElementID.closest(inputParentSelector).find('input[data-type="value"]').val('');
}

/* BEGIN image manipulation */
function process_image(event)
{
	event.stopPropagation();
	event.preventDefault();

	// allow image manipulation only if the editor is in "edit" state
	if($('#vCardEditor').attr('data-editor-state')!="edit")
		return false;

	if(typeof event.originalEvent.dataTransfer!='undefined')
		var files=event.originalEvent.dataTransfer.files; // fileList object from drag&drop
	else
		var files=event.originalEvent.target.files; // fileList object from input type file

	// files is a FileList of File objects. List some properties.
	for(var i=0;i<files.length;i++)	// we handle only the first picture here ... (see below)
	{
		// only process image files
		if(!files[i].type.match(/image/i))
			continue;

		// do not accept images bigger than 64KiB
		// if(files[i].size>65536)
		// 	continue;

		// show the image "delete" button
		$('#reset_img').css('display', 'inline');
		// remove the template related to previous image (start with clean one)
		vCard.tplM['contentline_PHOTO'][0]=null;

		var reader=new FileReader();
		// closure to capture the file information.
		reader.onload=(function(theFile){
			return function(e){
				//escape(files[i].name), files[i].type, files[i].size, files[i].lastModifiedDate
				var newImg=new Image();
				newImg.src=e.target.result;
				newImg.onload=function(){
					loadImage(this);
				};
			};
		})(files[i]);

		reader.readAsDataURL(files[i]);
		break; // we handle only the first picture here ...
	}

	$('#photoURL, #photoURLHidden').val('');
}
/* END image manipulation */


function hideNotVisibleMessage()
{
	globalAddressbookList.contactToReload=null;
	animate_message('#ABInMessageEditBox', '#ABInMessageTextEditBox', 0, '-=');
	$('#ABInMessageEditBox').css('display','');
}

function initSearchCardDav()
{
	if(globalQs==null)
	{
		$('#SearchBox').find('input[data-type="search"]').keyup(function(){
			globalAddressbookList.contactToReload=null
		});
		globalQs=$('#SearchBox').find('input[data-type="search"]').quicksearch(globalAddressbookList.contacts,
		{
			delay: 250,
			hide: function(){
				var tmp=$(this)[0];
				if(!tmp.headerOnly)
					tmp.search_hide=true;
			},
			show: function(){
				var tmp=$(this)[0];
				if(!tmp.headerOnly)
					tmp.search_hide=false;
			},
			prepareQuery: function (val){
				return val.multiReplace(globalSearchTransformAlphabet).toLowerCase().split(' ');
			},
			onBefore: function(){
				if($('#SearchBox').find('input[data-type="search"]').val()=='')
					$('#SearchBox').find('img[data-type="reset"]').css('display','none');
				else
					$('#SearchBox').find('img[data-type="reset"]').css('display','');
			},
			onAfter: function(){
				globalAddressbookList.applyABFilter(dataGetChecked('#ResourceCardDAVList'),false);
// XXX maybe this was the reason for data-filter-url?
//				globalAddressbookList.applyABFilter(globalRefAddContact.attr('data-filter-url'),false);

	// maybe useful for somebody
	//			if((selected_contact=globalRefABListTable.find('.ablist_item_selected')).length==1)
	//				globalRefABList.scrollTop(globalRefABList.scrollTop()+selected_contact.offset().top-globalRefABList.offset().top-globalRefABList.height()*globalKBNavigationPaddingRate);
			}
		});
	}
}

function initKbAddrNavigation()
{
	$(document.documentElement).keyup(function(event)
	{
		if(typeof globalActiveApp=='undefined' || globalActiveApp!='CardDavMATE')
			return true;

		if(globalActiveApp=='CardDavMATE' && globalObjectLoading==true)
		{
			event.preventDefault();
			return true;
		}

		//if($('#SystemCardDavMATE').css('display')!='none' && $('#ABListLoader').css('display')=='none' && $('#ABListOverlay').css('display')=='none' && !$('input[data-type="search"]').is(':focus'))
		/* XXX - System display:none changes */
		if($('#SystemCardDavMATE').css('visibility')!='hidden' && isCardDAVLoaded && $('#ABListOverlay').css('display')=='none' && !$('input[data-type="search"]').is(':focus'))
		{
			// 37 = left, 38 = up, 39 = right, 40 = down
			var selected_contact=null, next_contact=null;
			if((selected_contact=globalRefABListTable.find('.ablist_item_selected')).length==1)
			{
				if(event.keyCode == 38 && (next_contact=selected_contact.prevAll('.ablist_item').filter(':visible').first()).attr('data-id')!=undefined || event.keyCode == 40 && (next_contact=selected_contact.nextAll('.ablist_item').filter(':visible').first()).attr('data-id')!=undefined)
					globalAddressbookList.loadContactByUID(next_contact.attr('data-id'));
			}
		}
	});

	$(document.documentElement).keydown(function(event)
	{
		if(typeof globalActiveApp=='undefined' || globalActiveApp!='CardDavMATE')
			return true;

		if(globalActiveApp=='CardDavMATE' && globalObjectLoading==true)
		{
			event.preventDefault();
			return true;
		}

		//if($('#SystemCardDavMATE').css('display')!='none' && $('#ABListLoader').css('display')=='none' && $('#ABListOverlay').css('display')=='none' && !$('input[data-type="search"]').is(':focus'))
		/* XXX - System display:none changes */
		if($('#SystemCardDavMATE').css('visibility')!='hidden' && isCardDAVLoaded && $('#ABListOverlay').css('display')=='none' && !$('input[data-type="search"]').is(':focus'))
		{
			// 37 = left, 38 = up, 39 = right, 40 = down
			var selected_contact=null, next_contact=null;
			if((selected_contact=globalRefABListTable.find('.ablist_item_selected')).length==1)
			{
				var wrapperRef = $('.ablist_table_wrapper');

				if(event.keyCode == 38 && (next_contact=selected_contact.prevAll('.ablist_item').filter(':visible').first()).attr('data-id')!=undefined || event.keyCode == 40 &&  (next_contact=selected_contact.nextAll('.ablist_item').filter(':visible').first()).attr('data-id')!=undefined)
				{
					switch(event.keyCode)
					{
						case 38:
							event.preventDefault();
							if(wrapperRef.scrollTop()>wrapperRef.scrollTop()+next_contact.offset().top-wrapperRef.offset().top-wrapperRef.height()*globalKBNavigationPaddingRate)
								wrapperRef.scrollTop(wrapperRef.scrollTop()+next_contact.offset().top-wrapperRef.offset().top-wrapperRef.height()*globalKBNavigationPaddingRate);
							else if(wrapperRef.scrollTop()<wrapperRef.scrollTop()+next_contact.offset().top+next_contact.height()-wrapperRef.offset().top-wrapperRef.height()*(1-globalKBNavigationPaddingRate))	/* contact invisible (scrollbar moved) */
								wrapperRef.scrollTop(wrapperRef.scrollTop()+next_contact.offset().top+next_contact.height()-wrapperRef.offset().top-wrapperRef.height()*(1-globalKBNavigationPaddingRate));
							else
								return false;
							break;
						case 40:
							event.preventDefault();
							if(wrapperRef.scrollTop()<wrapperRef.scrollTop()+next_contact.offset().top+next_contact.height()-wrapperRef.offset().top-wrapperRef.height()*(1-globalKBNavigationPaddingRate))	/* contact invisible (scrollbar moved) */
								wrapperRef.scrollTop(wrapperRef.scrollTop()+next_contact.offset().top+next_contact.height()-wrapperRef.offset().top-wrapperRef.height()*(1-globalKBNavigationPaddingRate));
							else if(wrapperRef.scrollTop()>wrapperRef.scrollTop()+next_contact.offset().top-wrapperRef.offset().top-wrapperRef.height()*globalKBNavigationPaddingRate)
								wrapperRef.scrollTop(wrapperRef.scrollTop()+next_contact.offset().top-wrapperRef.offset().top-wrapperRef.height()*globalKBNavigationPaddingRate);
							else
								return false;
							break;
						default:
							break;
					}
				}
				else	// no previous contact and up pressed || no next contact and down pressed
				{
					switch(event.keyCode)
					{
						case 38:
							wrapperRef.scrollTop(0);
							break;
						case 40:
							wrapperRef.scrollTop(wrapperRef.prop('scrollHeight'));
							break;
						default:
							break;
					}
				}
			}
		}
	});
}

function initDatePicker(inputObject)
{
	if(!inputObject.hasClass('hasDatepicker'))
	{
		inputObject.datepicker({
			disabled: inputObject.prop('readonly') || inputObject.prop('disabled'),
			showMonthAfterYear: true,
			prevText: '',
			nextText: '',
			monthNamesShort: ['01','02','03','04','05','06','07','08','09','10','11','12'],
			dateFormat: globalSettings.datepickerformat.value,
			defaultDate: '-'+Math.round(30*365.25-1),
			minDate: '-120y',
			maxDate: '+0',
			yearRange: 'c-120:+0',
			firstDay: globalSettings.datepickerfirstdayofweek.value,
			weekendDays: globalSettings.weekenddays.value,
			changeMonth: true,
			changeYear: true,
			showAnim: '',
			afterUpdate: function(inst)
			{
				/*************************** BAD HACKS SECTION ***************************/
				// IE and FF datepicker selectbox problem fix
				if($.browser.msie || $.browser.mozilla)
				{
					var calendar=inst.dpDiv;
					setTimeout(function(){
						if($.browser.msie && parseInt($.browser.version, 10)==10)	/* IE 10 */
							calendar.find('select').css({'padding-top': '1px', 'padding-left': '0px', 'padding-right': '0px'});

						var newSVG=$(SVG_select).attr('data-type', 'select_icon').css({'pointer-events': 'none', 'z-index': '1', 'display': 'inline', 'margin-left': '-19px', 'vertical-align': 'top', 'background-color': '#ffffff'});	// background-color = stupid IE9 bug
						calendar.find('select').after($($('<div>').append($(newSVG).clone()).html()));
					},1);
				}
				else if(navigator.platform.toLowerCase().indexOf('win')==0 && $.browser.webkit && !!window.chrome)	/* Chrome on Windows */
				{
					var calendar=inst.dpDiv;
					setTimeout(function(){ calendar.find('select').css({'padding-left': '0px', 'padding-right': '13px'}); },1);
				}
				/*************************** END OF BAD HACKS SECTION ***************************/
			},
			beforeShow: function(input, inst)	// set the datepicker value if the date is out of range (min/max)
			{
				inst.dpDiv.removeClass('ui-datepicker-simple');

				var valid=true;
				try {var currentDate=$.datepicker.parseDate(globalSettings.datepickerformat.value, inputObject.val())}
				catch (e) {valid=false}

				if(valid==true && currentDate!=null)
				{
					var minDateText=inputObject.datepicker('option', 'dateFormat', globalSettings.datepickerformat.value).datepicker('option', 'minDate');
					var maxDateText=inputObject.datepicker('option', 'dateFormat', globalSettings.datepickerformat.value).datepicker('option', 'maxDate');

					var minDate=$.datepicker.parseDate(globalSettings.datepickerformat.value, minDateText);
					var maxDate=$.datepicker.parseDate(globalSettings.datepickerformat.value, maxDateText);

					if(currentDate<minDate)
						inputObject.val(minDateText);
					else if(currentDate>maxDate)
						inputObject.val(maxDateText);
				}

				// Timepicker hack (prevent IE to re-open the datepicker on date click + focus)
				var index=inputObject.attr("data-type");
				var d = new Date();
				if(globalTmpTimePickerHackTime[index]!=undefined && d.getTime()-globalTmpTimePickerHackTime[index]<200)
					return false;
			},
			onClose: function(dateText, inst)	// set the datepicker value if the date is out of range (min/max) and reset the value to proper format (for example 'yy-mm-dd' allows '2000-1-1' -> we need to reset the value to '2000-01-01')
			{
				var valid=true;
				try {var currentDate=$.datepicker.parseDate(globalSettings.datepickerformat.value, dateText)}
				catch (e) {valid=false}

				if(valid==true && currentDate!=null)
				{
					var minDateText=inputObject.datepicker('option', 'dateFormat', globalSettings.datepickerformat.value).datepicker('option', 'minDate');
					var maxDateText=inputObject.datepicker('option', 'dateFormat', globalSettings.datepickerformat.value).datepicker('option', 'maxDate');

					var minDate=$.datepicker.parseDate(globalSettings.datepickerformat.value, minDateText);
					var maxDate=$.datepicker.parseDate(globalSettings.datepickerformat.value, maxDateText);

					if(currentDate<minDate)
						inputObject.val(minDateText);
					else if(currentDate>maxDate)
						inputObject.val(maxDateText);
					else
						inputObject.val($.datepicker.formatDate(globalSettings.datepickerformat.value, currentDate));
				}

				// Timepicker hack (prevent IE to re-open the datepicker on date click + focus)
				var index=inputObject.attr("data-type");
				var d = new Date();
				globalTmpTimePickerHackTime[index]=d.getTime();

				inputObject.focus();

				if(inputObject.closest('tr').attr('data-attr-name')==='X-ABDATE') {
					inputObject.trigger('keyup');
				}
			}
		});

		inputObject.mousedown(function(){
			if(inputObject.datepicker('widget').css('display')=='none')
				inputObject.datepicker('show');
			else
				inputObject.datepicker('hide');
		});

		inputObject.on('keydown', function(event){
			// show datepicker on keydown (up/down/left/right) but only if it not causes cursor position move
			if(this.selectionStart!=undefined && this.selectionStart!=-1)
				if(((event.which==38 || event.which==37) && this.selectionStart==0) || ((event.which==40 || event.which==39) && this.selectionStart==$(this).val().length))
				{
					if(inputObject.datepicker('widget').css('display')=='none')
						inputObject.datepicker('show');
					else
						inputObject.datepicker('hide');
				}
		});

		inputObject.blur(function(event){
			// handle onblur event because datepicker can be already closed
			// note: because onblur is called more than once we can handle it only if there is a value change!
			var valid=true;
			try {var currentDate=$.datepicker.parseDate(globalSettings.datepickerformat.value, inputObject.val())}
			catch (e) {valid=false}

			if(valid==true && inputObject.val()!=$.datepicker.formatDate(globalSettings.datepickerformat.value, currentDate))
			{
				var minDateText=inputObject.datepicker('option', 'dateFormat', globalSettings.datepickerformat.value).datepicker('option', 'minDate');
				var maxDateText=inputObject.datepicker('option', 'dateFormat', globalSettings.datepickerformat.value).datepicker('option', 'maxDate');

				var minDate=$.datepicker.parseDate(globalSettings.datepickerformat.value, minDateText);
				var maxDate=$.datepicker.parseDate(globalSettings.datepickerformat.value, maxDateText);

				if(currentDate<minDate)
					inputObject.val(minDateText);
				else if(currentDate>maxDate)
					inputObject.val(maxDateText);
				else
					inputObject.val($.datepicker.formatDate(globalSettings.datepickerformat.value, currentDate));
			}
		});

		inputObject.on('keyup change', function(){
			if(!$(this).prop('readonly') && !$(this).prop('disabled'))
			{
				var valid=true;

				if($(this).val()!='')
				{
					try {$.datepicker.parseDate(globalSettings.datepickerformat.value, $(this).val())}
					catch (e) {valid=false}
				}

				if(valid)
					$(this).parent().find('img').css('display','none');
				else
					$(this).parent().find('img').css('display','inline');
			}
		});

		// show the datepicker after the initialization
		inputObject.datepicker('show');
	}
}

function checkForVcardGroups(contactUID)
{
	if($('#vCardEditor').attr('data-url')==contactUID)
	{
		var collUID= contactUID.replace(RegExp('[^/]*$'),'');
		var select_elem=$('#vCardEditor').find('[data-attr-name="_DEST_"]').find('[data-type="'+jqueryEscapeSelector(collUID)+'"]');
		if(select_elem.length==1)
		{
			var vGroupC = globalAddressbookList.getMyContactGroups(contactUID).length;
			if(vGroupC>1)
				select_elem.text(localization[globalInterfaceLanguage].txtVcardGroupsTextMulti.replace('%coll%',globalResourceCardDAVList.getCollectionByUID(collUID).displayvalue).replace('%n%',vGroupC));
			else if(vGroupC==1)
				select_elem.text(localization[globalInterfaceLanguage].txtVcardGroupsTextSingle.replace('%coll%',globalResourceCardDAVList.getCollectionByUID(collUID).displayvalue));
		}
	}
}

function checkContactFormScrollBar()
{
	var baseWidth = 582;
	var scrollWidth = $('#EditorBox').length ? $('#ABContact').outerWidth() - $('#EditorBox').outerWidth() : 0;
	var previousWidth = parseInt($('#ABList').css('right'), 10);
	var newWidth = baseWidth+scrollWidth;

	if(previousWidth===newWidth)
		return true;

	$('.collection_d, #SearchBox, #ABList').css('right', newWidth);
	$('#ABListOverlay').css('right', newWidth+1);
	$('.contact_d, #ABMessage, #ABContactOverlay').width(newWidth);
	$('#ABContactColor').css('right', newWidth-3);
	$('#ABContact').width(newWidth-3);

	var columnLengths = [];
	for(var i=0; i<getDataColumnCount(); i++) {
		columnLengths.push([]);
	}

	globalRefABListTable.children('.ablist_item:visible').each(function() {
		$(this).children().slice(globalFixedContactDataColumnsCount).each(function(ind) {
			columnLengths[ind].push($(this).text().length);
		});
	});

	setDataColumnsWidth(columnLengths);
}

function extendDestSelect(selGroup)
{
	if($('#vCardEditor').attr('data-editor-state')=='edit')
		return false;
	var dest = $('[data-attr-name="_DEST_"]');
	$('#ExtendedDest').remove();
	var extendedDest = $('<div id="ExtendedDest">');
	var destSelected = dest.children(':selected');
	var header = null;
	var headerShown = false;
	var currentGroups = typeof $('#vCardEditor').attr('data-vcard-uid')=='undefined' ? [] : globalAddressbookList.getMyContactGroups($('#vCardEditor').attr('data-url'));

	dest.parent().after(extendedDest);
	for(var i=0; i<globalResourceCardDAVList.collections.length; i++) {
		var resource = globalResourceCardDAVList.collections[i];
		if(typeof resource.headerOnly!='undefined' && resource.headerOnly) {
			header = resource;
			headerShown = false;
		}
		else if(typeof resource.makeLoaded!='undefined' && resource.makeLoaded) {
			if(!headerShown) {
				$('<div>').addClass('extended_dest_header').text(header.displayvalue).appendTo(extendedDest);
				headerShown = true;
			}

			var itemEl = $('<div>').addClass('extended_dest_item');
			var resourceEl = $('<div>').addClass('extended_dest_resource').text(resource.displayvalue);
			var groupContEl = $('<div>').addClass('extended_dest_group_container');

			$('<input>').attr({'type':'checkbox','data-id':resource.uid})
				.prop('checked',resource.uid==destSelected.attr('data-type'))
				.change(function(){
					if($(this).prop('checked')) {
						var newCollection = globalResourceCardDAVList.getCollectionByUID($(this).attr('data-id'));
						$(this).parent().parent().siblings().find('input[type="checkbox"]').prop('checked',false);
						dest.children('[data-type="'+newCollection.uid+'"]').prop('selected',true).text(newCollection.displayvalue);
						$('#ABContactColor').css('background-color',newCollection.color);
					}
					else
						$(this).prop('checked',true);
				})
				.prependTo(resourceEl);
			$('<div>').addClass('extended_dest_resource_color').css('background-color',resource.color).prependTo(resourceEl);

			for(var j=0; j<globalAddressbookList.vcard_groups[resource.uid].length; j++) {
				var group = globalAddressbookList.vcard_groups[resource.uid][j];
				var groupEl = $('<div>').addClass('extended_dest_group').text(group.displayvalue);

				$('<input>').attr({'type':'checkbox','data-id':group.uid})
					.prop('checked',currentGroups.indexOf(group.uid)!=-1 || typeof selGroup!= 'undefined' && selGroup==group.uid)
					.change(function(){
						var groupCount = $(this).parent().parent().find('input[type="checkbox"]:checked').length;
						var newCollectionUID = $(this).parent().parent().prev().children('input[type="checkbox"]').attr('data-id');
						var newCollection = globalResourceCardDAVList.getCollectionByUID(newCollectionUID);
						if(groupCount>1)
							dest.children('[data-type="'+newCollectionUID+'"]').text(localization[globalInterfaceLanguage].txtVcardGroupsTextMulti.replace('%coll%',newCollection.displayvalue).replace('%n%',groupCount));
						else if(groupCount==1)
							dest.children('[data-type="'+newCollectionUID+'"]').text(localization[globalInterfaceLanguage].txtVcardGroupsTextSingle.replace('%coll%',newCollection.displayvalue));
						else
							dest.children('[data-type="'+newCollectionUID+'"]').text(newCollection.displayvalue);

						if($(this).prop('checked')) {
							$(this).parent().parent().prev().children('input[type="checkbox"]').prop('checked',true);
							$(this).parent().parent().parent().siblings().find('input[type="checkbox"]').prop('checked',false);
							dest.children('[data-type="'+newCollectionUID+'"]').prop('selected',true);
							$('#ABContactColor').css('background-color',newCollection.color);
						}
					})
					.prependTo(groupEl);
				$('<div>').addClass('extended_dest_group_color').css('background-color',group.color).prependTo(groupEl);

					groupEl.appendTo(groupContEl);
			}

			resourceEl.appendTo(itemEl);
			groupContEl.appendTo(itemEl);
			itemEl.appendTo(extendedDest);
		}
	}

	dest.mousedown(function(e){
		e.stopPropagation();
		e.preventDefault();
		this.blur();

		if(extendedDest.height()>0) {
			dest.removeClass('inverse_select');
			/*************************** BAD HACKS SECTION ***************************/
			if($.browser.msie || $.browser.mozilla)
			{
				var newSVG=$(SVG_select).attr('data-type', 'select_icon').css({'pointer-events': 'none', 'z-index': '1', 'display': 'inline', 'margin-left': '-19px', 'vertical-align': 'top', 'background-color': '#ffffff'});	// background-color = stupid IE9 bug
				dest.parent().find('svg[data-type="select_icon"]').replaceWith($('<div>').append($(newSVG).clone()).html());
			}
			/*************************** END OF BAD HACKS SECTION ***************************/
			extendedDest.animate({'height':0},200);
			$('html').unbind('mousedown');
		}
		else {
			dest.addClass('inverse_select');
			/*************************** BAD HACKS SECTION ***************************/
			if($.browser.msie || $.browser.mozilla)
			{
				var newSVG=$(SVG_select_inv).attr('data-type', 'select_icon').css({'pointer-events': 'none', 'z-index': '1', 'display': 'inline', 'margin-left': '-19px', 'vertical-align': 'top', 'background-color': '#ffffff'});	// background-color = stupid IE9 bug
				dest.parent().find('svg[data-type="select_icon"]').replaceWith($('<div>').append($(newSVG).clone()).html());
			}
			/*************************** END OF BAD HACKS SECTION ***************************/
			extendedDest.animate({'height':164},200);
			$('html').mousedown(function(e){
				if(e.target.id=='ExtendedDest' || $.contains(document.getElementById('ExtendedDest'),e.target))
					return true;

				dest.removeClass('inverse_select');
				/*************************** BAD HACKS SECTION ***************************/
				if($.browser.msie || $.browser.mozilla)
				{
					var newSVG=$(SVG_select).attr('data-type', 'select_icon').css({'pointer-events': 'none', 'z-index': '1', 'display': 'inline', 'margin-left': '-19px', 'vertical-align': 'top', 'background-color': '#ffffff'});	// background-color = stupid IE9 bug
					dest.parent().find('svg[data-type="select_icon"]').replaceWith($('<div>').append($(newSVG).clone()).html());
				}
				/*************************** END OF BAD HACKS SECTION ***************************/
				extendedDest.animate({'height':0},200);
				$('html').unbind('mousedown');
			});
		}
	});
}

/*
$(document).on("mouseover", "#vCardEditor .ablist_item", function() {
	if(!$(this).is('.ui-draggable') && (typeof globalDisableDragAndDrop=='undefined' || globalDisableDragAndDrop!=true))
	{
		$(this).draggable({
			delay: 250,
			revert: 'invalid',
			scroll: false,
			opacity: 0.8,
			stack: '#SystemCardDavMATE',
			containment: '#SystemCardDavMATE',
			appendTo: 'body',
			start: function( event, ui ){
				// disallow on read-only collection
				if(globalResourceCardDAVList.getCollectionPrivByUID($(this).attr('data-id').replace(RegExp('[^/]*$'),''))==true)
					return false;
			},
			helper: function(){
				$('#ResourceCardDAVList').find('.resourceCardDAV.ui-droppable').droppable( 'option', 'accept', false);
				$('#ResourceCardDAVList').find('.group.ui-droppable').droppable( 'option', 'accept', false);

				$('#ResourceCardDAVList').find('.resourceCardDAV[data-id!='+jqueryEscapeSelector($(this).attr('data-id').replace(RegExp('[^/]+$'),''))+'].ui-droppable').droppable( 'option', 'accept', '.ablist_item');
				var myContactGroups=globalAddressbookList.getMyContactGroups($(this).attr('data-id'));
				$('#ResourceCardDAVList').find('.group[data-id^='+jqueryEscapeSelector($(this).attr('data-id').replace(RegExp('[^/]+$'),''))+'].ui-droppable').not('.resourceCardDAV_selected').each(function(index, element){
					if(myContactGroups.indexOf($(element).attr('data-id'))==-1)
						$(element).droppable( 'option', 'accept', '.ablist_item');
				});

				var tmp=$(this).clone();
				tmp.addClass('ablist_item_dragged');
				// we cannot use .css() here, because we need to add !important (problem with Gecko based browsers)
				var tmp_style='max-width: '+$(this).outerWidth()+'px;';
				if($(this).css('background-image')!='none')
					tmp_style+='background-image: url(images/company_s_w.svg) !important;';
				tmp.attr('style', tmp_style);

				return tmp;
			}
		});
	}
});
*/

function setDataColumnsWidth(cache) {
	if(!globalRefABListTableCols && !globalRefABListInnerTableCols) {
		return true;
	}

	// remove gutter
	$('.ablist_table_gutter').remove();

	// clear old column widths
	globalRefABListTableCols.width('');
	globalRefABListInnerTableCols.width('');

	// use cached column values to compute new column widths
	var characterWidth = 9; // gross approximation
	var lastColumn = null;
	var lastInnerColumn = null;
	var scrollWidth = $('.ablist_table_wrapper').innerWidth() - globalRefABListTable.outerWidth();
	var reservedWidth = 0;
	globalRefABListTable.children('.ablist_item').first().children().slice(0, globalFixedContactDataColumnsCount).each(function() {
		reservedWidth += $(this).width();
	});
	var availableWidth;
	var maxWidth;
	availableWidth = maxWidth = globalRefABList.innerWidth() - reservedWidth - scrollWidth;

	cache.every(function(lengths, index) {
		// var maxLength = Math.max.apply(null, lengths);
		lengths.sort(function(a, b) {
			return a - b;
		});

		var maxLength = lengths[Math.max(Math.min(Math.ceil(lengths.length * globalContactDataMinVisiblePercentage), lengths.length) - 1, 0)];
		var column = globalRefABListTableCols.eq(index + globalFixedContactDataColumnsCount);
		var innerColumn = globalRefABListInnerTableCols.eq(index + globalFixedContactDataColumnsCount);
		var columnWidth = Math.max(maxLength * characterWidth, getDataColumnMinWidthAtIndex(index));

		// exit early if there is not enough space for the column
		if(columnWidth > availableWidth) {
			// if exiting at the very first column, mark it as the last visible one anyway
			// this will ensure that it gets to occupy what width there is available later on
			if(!lastColumn) {
				lastColumn = column;
			}
			if(!lastInnerColumn) {
				lastInnerColumn = innerColumn;
			}

			return false;
		}

		// dont show column if no data are present
		if(columnWidth && lengths[lengths.length - 1]>0) {
			lastColumn = column;
			lastInnerColumn = innerColumn;
			availableWidth -= columnWidth;
			column.width(columnWidth);
			innerColumn.width(columnWidth);
		}

		return true;
	});

	// set the last visible column to occupy the rest of the available table width
	if(lastColumn && lastInnerColumn) {
		lastColumn.width(lastColumn.width() + availableWidth);
		lastInnerColumn.width(lastInnerColumn.width() + availableWidth);
	}

	// if scrollbar present, create gutter
	if(scrollWidth) {
		$('<col class="ablist_table_gutter">').width(scrollWidth).insertAfter(lastColumn);
		$('<th class="ablist_table_gutter">').insertAfter($('.ablist_table_header').children().eq(lastColumn.index()));
	}
}

function getDataColumnCount() {
	return globalSettings.collectiondisplay.value.length;
}

function isDataColumnDefined(column) {
	var re = RegExp('(?:^|[^\\\\]){'+column+'(?:\\[.*?\\])*'+'}', 'i');

	return globalSettings.collectiondisplay.value.some(function(col) {
		if(col.hasOwnProperty('value')) {
			var values = col.value;

			if($.isPlainObject(values)) {
				return values.company.some(function(value) {
					return re.test(value)
				}) || values.personal.some(function(value) {
					return re.test(value)
				});
			}

			return values.some(function(value) {
				return re.test(value)
			});
		}

		return false;
	});
}

function getContactDataColumns(isCompany) {
	return $.map(globalSettings.collectiondisplay.value, function(col) {
		var value = col.value;

		if($.isPlainObject(value)) {
			if(isCompany && value.hasOwnProperty('company')) {
				return [value.company];
			}
			if(!isCompany && value.hasOwnProperty('personal')) {
				return [value.personal];
			}
		}

		return [value];
	});
}

function getDataColumnLabelAtIndex(index) {
	if(globalSettings.collectiondisplay.value[index].hasOwnProperty('label')) {
		var label = globalSettings.collectiondisplay.value[index].label;

		if($.isPlainObject(label)) {
			return getDataColumnLabel(label[globalInterfaceLanguage] || '');
		}
		else {
			return getDataColumnLabel(label);
		}
	}
}

function getDataColumnLabel(formatString) {
	var result = '';
	var variableParts = null;
	var re = RegExp('(?:^|[^\\\\])({(.*?[^\\\\])})');

	while(variableParts = formatString.match(re)) {
		var value = localization[globalInterfaceLanguage][globalContactDataColumnLabelVars[variableParts[2]]] || '';
		formatString = formatString.replace(variableParts[1], value);
	}

	return formatString;
}

function getDataColumnMinWidthAtIndex(index) {
	return 100;
}

function setContactDataColumn(contact, column, value, filterData) {
	var column = column.toUpperCase();

	if(globalContactDataColumnDefs.hasOwnProperty(column) && value) {
		var property = globalContactDataColumnDefs[column].property;

		if(!contact.hasOwnProperty(property)) {
			contact[property] = [];
		}

		var data = {};

		for(var name in filterData) {
			var filterProperty = globalContactDataColumnDefs[column].filterProperities[name];
			data[filterProperty] = filterData[name];
		}

		if($.isArray(value)) {
			value = value.join(', ');
		}
		data.value = value;

		contact[property].push(data);
	}
}

function getContactDataColumn(contact, variables) {
	var result = '';
	var variableParts = null;
	var re = RegExp('(?:^|[^\\\\])({(.*?[^\\\\])})');

	variables.forEach(function(formatString) {
		var matched = false;

		while(variableParts = formatString.match(re)) {
			var value = getContactDataColumnVariable(contact, variableParts[2]);
			formatString = formatString.replace(variableParts[1], value);
			matched = matched || value!=='';
		}

		if(matched) {
			 result += formatString;
		}
	});

	return result;
}

function getContactDataColumnVariable(contact, variable) {
	var parts = variable.match(/^(.*?)(\[.*\])*$/);
	var attr = parts[1].toUpperCase();

	if(parts && attr && globalContactDataColumnDefs.hasOwnProperty(attr)) {
		var property = globalContactDataColumnDefs[attr].property;

		if(contact.hasOwnProperty(property)) {
			var re = RegExp('\\[(.*?[^\\\\])\\]');
			var numeral = 0;
			var filterStr = parts[2] ? parts[2].toUpperCase() : '';
			var filters = [];
			var matches = contact[property];

			while(filterStr) {
				var match = filterStr.match(re);

				if(match===null) {
					break;
				}

				filters.push(match[1].replaceAll('\\[', '[').replaceAll('\\]', ']'));
				filterStr = filterStr.replace(match[0], '');
			}

			filters.forEach(function(filterEl) {
				if(filterEl[0]===':') {
					numeral = parseInt(filterEl.slice(1), 10);
				}
				else {
					var filterParts = filterEl.splitCustom('=');
					var filterType = filterParts[0];
					var filterValue = filterParts[1];

					if(filterType && filterValue && globalContactDataColumnDefs[attr].hasOwnProperty('filterProperities') && globalContactDataColumnDefs[attr].filterProperities.hasOwnProperty(filterType)) {
						var filterProperty = globalContactDataColumnDefs[attr].filterProperities[filterType];

						matches = matches.filter(function(matchEl) {
							return matchEl[filterProperty].indexOf(filterValue)>-1;
						});
					}
				}
			});

			if(!isNaN(numeral) && numeral>-1 && numeral<matches.length) {
				return matches[numeral].value;
			}
		}
	}

	return '';
}

function getParamsFromContentlineParse(vcard, parsed, primaryParam, customParam, dataTypeRegister, preserveCase) {
	var params = [];

	if(primaryParam && parsed[3]) {
		var parsed_paramArr = vcardSplitValue(parsed[3], ';');

		parsed_paramArr.forEach(function(el) {
			if(el) {
				var elParts = el.split('=');

				if(elParts[0].toUpperCase()===primaryParam) {
					var val = elParts[1];

					if(!preserveCase) {
						val = val.toUpperCase();
					}

					params.push(humanizeVcardDataTypes(dataTypeRegister, val));
				}
			}
		});
	}
	if(customParam && parsed[1]) {
		var vcard_element_related = null;
		var re = RegExp('\r\n'+parsed[1].replace('.','\\.'+customParam+':(.*)')+'\r\n', 'im');
		while((vcard_element_related = vcard.match(re))!=null) {
			var val = vcard_element_related[1];

			if(!preserveCase) {
				val = val.toUpperCase();
			}

			params.push(humanizeVcardDataTypes(dataTypeRegister, vcardUnescapeValue(val)));
			vcard = vcard.replace(vcard_element_related[0], '\r\n');
		}
	}

	return params;
}

function humanizeVcardDataTypes(register, type) {
	if(register && dataTypes[register].hasOwnProperty(type.toLowerCase())) {
		matched = type.match(/^_\$!<(.*)>!\$_$/i);

		if(matched) {
			return matched[1];
		}
	}

	return type;
}

function showPhotoBox(e) {
	if($('#photoBox').is(':visible'))
		hidePhotoBox();
	else
	{
		e.stopPropagation();

		$('#photoArrow, #photoBox').css('display', 'block');
		$('#photoURL').focus();

		$('html').bind('click.photo', function(e) {
			if(!$.contains(document.getElementById('photoBox'), e.target)) {
				hidePhotoBox();
			}
		});
	}
}

function hidePhotoBox() {
	$('#photoURL').val($('#photoURLHidden').val());
	$('#photoBoxContent').find('[data-type="invalid"]').css('display', 'none');
	$('#photoURL').removeClass('invalid');

	$('#photoBox').css('display','none');
	$('#photoArrow').css('display','none');
	$('html').unbind('click.photo');
}
