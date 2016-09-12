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

function dataToVcard(accountUID, inputUID, inputFilterUID, inputEtag)
{
	var vCardText='';
	var groupCounter=0;
	var tmpvCardEditorRef=$('#vCardEditor');
	if(typeof globalDisabledContactAttributes=='undefined' || !(globalDisabledContactAttributes instanceof Array))
		globalDisabledContactAttributes=[];

	// vCard BEGIN (required by RFC)
	if(vCard.tplM['begin']!=null && (process_elem=vCard.tplM['begin'][0])!=undefined)
		vCardText+=vCard.tplM['begin'][0];
	else
	{
		process_elem=vCard.tplC['begin'];
		process_elem=process_elem.replace('##:::##group_wd##:::##','');
		vCardText+=process_elem;
	}

// VERSION (required by RFC)
	if(vCard.tplM['contentline_VERSION']!=null && (process_elem=vCard.tplM['contentline_VERSION'][0])!=undefined)
	{
		// replace the object and related objects' group names (+ append the related objects after the processed)
		parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
		if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
			process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
	}
	else
	{
		process_elem=vCard.tplC['contentline_VERSION'];
		process_elem=process_elem.replace('##:::##group_wd##:::##', '');
		process_elem=process_elem.replace('##:::##version##:::##', '3.0');
	}
	vCardText+=process_elem;

// UID (required by RFC)
	var newUID='';
	if(vCard.tplM['contentline_UID']!=null && (process_elem=vCard.tplM['contentline_UID'][0])!=undefined)
	{
		// replace the object and related objects' group names (+ append the related objects after the processed)
		parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
		if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
			process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
	}
	else
	{
		process_elem=vCard.tplC['contentline_UID'];
		process_elem=process_elem.replace('##:::##group_wd##:::##', '');
		process_elem=process_elem.replace('##:::##params_wsc##:::##', '');

		newUID=globalAddressbookList.getNewUID();

		// it is VERY small probability, that for 2 newly created contacts the same UID is generated (but not impossible :( ...)
		process_elem=process_elem.replace('##:::##uid##:::##',newUID);
	}
	vCardText+=process_elem;

// N (required by RFC)
	if(vCard.tplM['contentline_N']!=null && (process_elem=vCard.tplM['contentline_N'][0])!=undefined)
	{
		// replace the object and related objects' group names (+ append the related objects after the processed)
		parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
		if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
			process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
	}
	else
	{
		process_elem=vCard.tplC['contentline_N'];
		process_elem=process_elem.replace('##:::##group_wd##:::##', '');
		process_elem=process_elem.replace('##:::##params_wsc##:::##', '');
	}
	process_elem=process_elem.replace('##:::##family##:::##',vcardEscapeValue(tmpvCardEditorRef.find('[data-type="family"]').val()));
	process_elem=process_elem.replace('##:::##given##:::##',vcardEscapeValue(tmpvCardEditorRef.find('[data-type="given"]').val()));
	process_elem=process_elem.replace('##:::##middle##:::##',vcardEscapeValue(tmpvCardEditorRef.find('[data-type="middle"]').val()));
	process_elem=process_elem.replace('##:::##prefix##:::##',vcardEscapeValue(tmpvCardEditorRef.find('[data-type="prefix"]').val()));
	process_elem=process_elem.replace('##:::##suffix##:::##',vcardEscapeValue(tmpvCardEditorRef.find('[data-type="suffix"]').val()));
	vCardText+=process_elem;

// FN (extracted from newly created N [previous "process_elem"], required by RFC)
	// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
	parsed=('\r\n'+process_elem).match(vCard.pre['contentline_parse']);
	// parsed_value = [0]->Family, [1]->Given, [2]->Middle, [3]->Prefix, [4]->Suffix
	parsed_value=vcardSplitValue(parsed[4],';');

// XXX toto je blbost, v settingsoch predsa musi byt jednoznacne ci sa uklada format A alebo B
	/* backward compatibility for stupid users (remove it in future) */
	if(typeof globalSettings.contactstorefn.value=='string')
		var tmp=globalSettings.contactstorefn.value.replace(RegExp(',', 'g'),', ').split(',');
	else	/* new configuration options (arrays) */
		var tmp=globalSettings.contactstorefn.value.slice();	// copy the configuration array

	var first_found=false;
	for(var i=0;i<tmp.length;i++)
	{
		var tmp_found=false;
		if(tmp[i].match(RegExp('surname|lastname|last|family','ig'))!=null)
		{
			if(parsed_value[0]=='')
				tmp[i]='';
			else
			{
				tmp[i]=tmp[i].replace(RegExp((!first_found ? '.*' : '')+'(surname|lastname|last|family)','ig'),parsed_value[0]);
				first_found=true;
			}
		}
		if(tmp[i].match(RegExp('firstname|first|given','ig'))!=null)
		{
			if(parsed_value[1]=='')
				tmp[i]='';
			else
			{
				tmp[i]=tmp[i].replace(RegExp((!first_found ? '.*' : '')+'(firstname|first|given)','ig'),parsed_value[1]);
				first_found=true;
			}
		}
		if(tmp[i].match(RegExp('middlename|middle','ig'))!=null)
		{
			if(parsed_value[2]=='')
				tmp[i]='';
			else
			{
				tmp[i]=tmp[i].replace(RegExp((!first_found ? '.*' : '')+'(middlename|middle)','ig'),parsed_value[2]);
				first_found=true;
			}
		}
		if(tmp[i].match(RegExp('prefix','ig'))!=null)
		{
			if(parsed_value[3]=='')
				tmp[i]='';
			else
			{
				tmp[i]=tmp[i].replace(RegExp((!first_found ? '.*' : '')+'prefix','ig'),parsed_value[3]);
				first_found=true;
			}
		}
		if(tmp[i].match(RegExp('suffix','ig'))!=null)
		{
			if(parsed_value[4]=='')
				tmp[i]='';
			else
			{
				tmp[i]=tmp[i].replace(RegExp((!first_found ? '.*' : '')+'suffix','ig'),parsed_value[4]);
				first_found=true;
			}
		}
	}
	fn_value=tmp.join('');

	if(fn_value=='')	//empty FN -> we use the company name as FN
		fn_value=vcardEscapeValue(tmpvCardEditorRef.find('[data-type="org"]').val());

	if(vCard.tplM['contentline_FN']!=null && (process_elem=vCard.tplM['contentline_FN'][0])!=undefined)
	{
		// replace the object and related objects' group names (+ append the related objects after the processed)
		parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
		if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
			process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
	}
	else
	{
		process_elem=vCard.tplC['contentline_FN'];
		process_elem=process_elem.replace('##:::##group_wd##:::##', '');
		process_elem=process_elem.replace('##:::##params_wsc##:::##', '');
	}
	process_elem=process_elem.replace('##:::##fn##:::##',fn_value);
	vCardText+=process_elem;

// CATEGORIES
	if(globalDisabledContactAttributes.indexOf('CATEGORIES')==-1 && (value=tmpvCardEditorRef.find('[data-type="\\%categories"]').find('input[data-type="value"]').val())!='')
	{
		if(vCard.tplM['contentline_CATEGORIES']!=null && (process_elem=vCard.tplM['contentline_CATEGORIES'][0])!=undefined)
		{
			// replace the object and related objects' group names (+ append the related objects after the processed)
			parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
			if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
				process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
		}
		else
		{
			process_elem=vCard.tplC['contentline_CATEGORIES'];
			process_elem=process_elem.replace('##:::##group_wd##:::##', '');
			process_elem=process_elem.replace('##:::##params_wsc##:::##', '');
		}
		process_elem=process_elem.replace('##:::##value##:::##', value);	// we do not need to escape the value here!
		vCardText+=process_elem;
	}

// NOTE
	if(globalDisabledContactAttributes.indexOf('NOTE')==-1 && (value=tmpvCardEditorRef.find('[data-type="\\%note"]').find('textarea').val())!='')
	{
		if(vCard.tplM['contentline_NOTE']!=null && (process_elem=vCard.tplM['contentline_NOTE'][0])!=undefined)
		{
			// replace the object and related objects' group names (+ append the related objects after the processed)
			parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
			if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
				process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
		}
		else
		{
			process_elem=vCard.tplC['contentline_NOTE'];
			process_elem=process_elem.replace('##:::##group_wd##:::##', '');
			process_elem=process_elem.replace('##:::##params_wsc##:::##', '');
		}
		process_elem=process_elem.replace('##:::##value##:::##',vcardEscapeValue(value));
		vCardText+=process_elem;
	}

// REV
	if(vCard.tplM['contentline_REV']!=null && (process_elem=vCard.tplM['contentline_REV'][0])!=undefined)
	{
		// replace the object and related objects' group names (+ append the related objects after the processed)
		parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
		if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
			process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
	}
	else
	{
		process_elem=vCard.tplC['contentline_REV'];
		process_elem=process_elem.replace('##:::##group_wd##:::##', '');
	}
	process_elem=process_elem.replace('##:::##params_wsc##:::##', '');
	var d = new Date();
	var utc=d.getUTCFullYear()+(d.getUTCMonth()+1<10 ? '0':'')+(d.getUTCMonth()+1)+(d.getUTCDate()<10 ? '0':'')+d.getUTCDate()+'T'+(d.getUTCHours()<10 ? '0':'')+d.getUTCHours()+(d.getUTCMinutes()<10 ? '0':'')+d.getUTCMinutes()+(d.getUTCSeconds()<10 ? '0':'')+d.getUTCSeconds()+'Z';
	process_elem=process_elem.replace('##:::##value##:::##', utc);
	vCardText+=process_elem;

// NICKNAME
	if(globalDisabledContactAttributes.indexOf('NICKNAME')==-1 && (value=tmpvCardEditorRef.find('[data-type="nickname"]').val())!='')
	{
		if(vCard.tplM['contentline_NICKNAME']!=null && (process_elem=vCard.tplM['contentline_NICKNAME'][0])!=undefined)
		{
			// replace the object and related objects' group names (+ append the related objects after the processed)
			parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
			if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
				process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
		}
		else
		{
			process_elem=vCard.tplC['contentline_NICKNAME'];
			process_elem=process_elem.replace('##:::##group_wd##:::##', '');
			process_elem=process_elem.replace('##:::##params_wsc##:::##', '');
		}
		process_elem=process_elem.replace('##:::##value##:::##', vcardEscapeValue(value));
		vCardText+=process_elem;
	}

// X-PHONETIC-FIRST-NAME
	if(globalDisabledContactAttributes.indexOf('X-PHONETIC-FIRST-NAME')==-1 && (value=tmpvCardEditorRef.find('[data-type="ph_firstname"]').val())!='')
	{
		if(vCard.tplM['contentline_X-PHONETIC-FIRST-NAME']!=null && (process_elem=vCard.tplM['contentline_X-PHONETIC-FIRST-NAME'][0])!=undefined)
		{
			// replace the object and related objects' group names (+ append the related objects after the processed)
			parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
			if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
				process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
		}
		else
		{
			process_elem=vCard.tplC['contentline_X-PHONETIC-FIRST-NAME'];
			process_elem=process_elem.replace('##:::##group_wd##:::##', '');
			process_elem=process_elem.replace('##:::##params_wsc##:::##', '');
		}
		process_elem=process_elem.replace('##:::##value##:::##',vcardEscapeValue(value));
		vCardText+=process_elem;
	}

// X-PHONETIC-LAST-NAME
	if(globalDisabledContactAttributes.indexOf('X-PHONETIC-LAST-NAME')==-1 && (value=tmpvCardEditorRef.find('[data-type="ph_lastname"]').val())!='')
	{
		if(vCard.tplM['contentline_X-PHONETIC-LAST-NAME']!=null && (process_elem=vCard.tplM['contentline_X-PHONETIC-LAST-NAME'][0])!=undefined)
		{
			// replace the object and related objects' group names (+ append the related objects after the processed)
			parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
			if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
				process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
		}
		else
		{
			process_elem=vCard.tplC['contentline_X-PHONETIC-LAST-NAME'];
			process_elem=process_elem.replace('##:::##group_wd##:::##', '');
			process_elem=process_elem.replace('##:::##params_wsc##:::##', '');
		}
		process_elem=process_elem.replace('##:::##value##:::##',vcardEscapeValue(value));
		vCardText+=process_elem;
	}

// BDAY
	if(globalDisabledContactAttributes.indexOf('BDAY')==-1 && (value=tmpvCardEditorRef.find('[data-type="date_bday"]').val())!='')
	{
		var valid=true;
		try {var date=$.datepicker.parseDate(globalSettings.datepickerformat.value, value)}
		catch (e) {valid=false}

		if(valid==true)
		{
			if(vCard.tplM['contentline_BDAY']!=null && (process_elem=vCard.tplM['contentline_BDAY'][0])!=undefined)
			{
				// replace the object and related objects' group names (+ append the related objects after the processed)
				parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
				if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
					process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
			}
			else
			{
				process_elem=vCard.tplC['contentline_BDAY'];
				process_elem=process_elem.replace('##:::##group_wd##:::##', '');
				process_elem=process_elem.replace('##:::##params_wsc##:::##', ';VALUE=date');
			}

			process_elem=process_elem.replace('##:::##value##:::##',vcardEscapeValue($.datepicker.formatDate('yy-mm-dd', date)));
			vCardText+=process_elem;
		}
	}

// X-ABDATE
	if(globalDisabledContactAttributes.indexOf('X-ABDATE')==-1)
	{
		tmpvCardEditorRef.find('[data-type="\\%date"]').each(
		function (index,element)
		{
			if((value=$(element).find('[data-type="date_value"]').val())!='')
			{
				var valid=true;
				try {var date=$.datepicker.parseDate(globalSettings.datepickerformat.value, value)}
				catch (e) {valid=false}

				if(valid==true)
				{
					incGroupCounter=false;
					if(vCard.tplM['contentline_X-ABDATE']!=null && (process_elem=vCard.tplM['contentline_X-ABDATE'][$(element).attr('data-id')])!=undefined)
					{
						// replace the object and related objects' group names (+ append the related objects after the processed)
						parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
						if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
						{
							process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter)+'.').substring(2);
							incGroupCounter=true;
						}
					}
					else
					{
						process_elem=vCard.tplC['contentline_X-ABDATE'];
						process_elem=process_elem.replace('##:::##group_wd##:::##', '');
					}
					var date_value=$.datepicker.formatDate('yy-mm-dd', date);

					var tmp_type=$(element).find('[data-type="date_type"] option').filter(':selected').attr('data-type');
					/* construct the "custom" type */
					if(tmp_type==':custom')
					{
						var tmp_cust_value=$(element).find('[data-type="custom_value"]').val();
						var tmp_cust_value_processed=tmp_cust_value.replace(RegExp('^\\s*|\\s*$','g'),'').replaceAll('  ',' ');
						// if a custom type is already defined as standard type, use the standard definition
						if((tmp_cust_already_exists=$(element).find('[data-type="date_type"] option').filter(function(){return $(this).html()==tmp_cust_value_processed;}).attr('data-type'))!=undefined)
							tmp_type=tmp_cust_already_exists;
						else	// use custom type
							tmp_type=':'+tmp_cust_value+':';
					}

					params_wsc='';
					tmp_normal_types=tmp_type.replace(RegExp(':.*:','g'),',').replaceAll(',,',',').replace(RegExp('^,|,$','g'),'');
					if(tmp_normal_types!='')
						params_wsc=';TYPE='+vcardEscapeValue(tmp_normal_types).toUpperCase().replace(RegExp('\\\\,','g'),';TYPE=');

					process_elem=process_elem.replace('##:::##params_wsc##:::##',params_wsc);
					process_elem=process_elem.replace('##:::##value##:::##',vcardEscapeValue(date_value));

					my_related='';
					tmp_related_type=tmp_type.match(RegExp(':(.*):'));	// only one element of related (X-ABLabel) is supported

					if(tmp_related_type!=null && tmp_related_type[1]!='')
						my_related='X-ABLabel:'+vcardEscapeValue((dataTypes['date_store_as'][tmp_related_type[1]]!=undefined ? dataTypes['date_store_as'][tmp_related_type[1]] : tmp_related_type[1]))+'\r\n';

					if(my_related!='')
					{
						incGroupCounter=true;
						parsed=('\r\n'+process_elem).match(vCard.pre['contentline_parse']);
						if(parsed[1]!='')	// if group is present, we use it, otherwise we create a new group
							process_elem+=parsed[1]+my_related;
						else
							process_elem='item'+groupCounter+'.'+process_elem+'item'+groupCounter+'.'+my_related;
					}

					if(incGroupCounter) groupCounter++;

					if(globalSettings.compatibility.value.anniversaryOutputFormat.indexOf('other')!=-1)
					{
						// X-ANNIVERSARY
						if(tmp_type==':_$!<anniversary>!$_:')
						{
							if(globalSettings.compatibility.value.anniversaryOutputFormat.indexOf('apple')!=-1)
								vCardText+=process_elem;
							process_elem='X-ANNIVERSARY;VALUE=date:'+vcardEscapeValue(date_value)+'\r\n';
						}

					}
					vCardText+=process_elem;
				}
			}
		});
	}

// TITLE
	if(globalDisabledContactAttributes.indexOf('TITLE')==-1 && (value=tmpvCardEditorRef.find('[data-type="title"]').val())!='')
	{
		if(vCard.tplM['contentline_TITLE']!=null && (process_elem=vCard.tplM['contentline_TITLE'][0])!=undefined)
		{
			// replace the object and related objects' group names (+ append the related objects after the processed)
			parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
			if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
				process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
		}
		else
		{
			process_elem=vCard.tplC['contentline_TITLE'];
			process_elem=process_elem.replace('##:::##group_wd##:::##', '');
			process_elem=process_elem.replace('##:::##params_wsc##:::##', '');
		}
		process_elem=process_elem.replace('##:::##value##:::##',vcardEscapeValue(value));
		vCardText+=process_elem;
	}

// ORG
	if(globalDisabledContactAttributes.indexOf('ORG')==-1)
	{
		value=tmpvCardEditorRef.find('[data-type="org"]:visible:not([readonly])').val();
		value2=tmpvCardEditorRef.find('[data-type="department"]:visible:not([readonly])').val();
		if((value!=undefined && value!='') || (value2!=undefined && value2!=''))
		{
			if(vCard.tplM['contentline_ORG']!=null && (process_elem=vCard.tplM['contentline_ORG'][0])!=undefined)
			{
				// replace the object and related objects' group names (+ append the related objects after the processed)
				parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
				if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
					process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
			}
			else
			{
				process_elem=vCard.tplC['contentline_ORG'];
				process_elem=process_elem.replace('##:::##group_wd##:::##', '');
				process_elem=process_elem.replace('##:::##params_wsc##:::##', '');
				process_elem=process_elem.replace('##:::##units_wsc##:::##', '');
			}
			process_elem=process_elem.replace('##:::##org##:::##',vcardEscapeValue(value)+(value2!=undefined && value2!='' ? ';'+vcardEscapeValue(value2) : ''));
			vCardText+=process_elem;
		}
	}

// X-ABShowAs
	if(globalDisabledContactAttributes.indexOf('X-ABShowAs')==-1 && tmpvCardEditorRef.find('[data-type="isorg"]').prop('checked'))
	{
		if(vCard.tplM['contentline_X-ABShowAs']!=null && (process_elem=vCard.tplM['contentline_X-ABShowAs'][0])!=undefined)
		{
			// replace the object and related objects' group names (+ append the related objects after the processed)
			parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
			if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
				process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);
		}
		else
		{
			process_elem=vCard.tplC['contentline_X-ABShowAs'];
			process_elem=process_elem.replace('##:::##group_wd##:::##', '');
			process_elem=process_elem.replace('##:::##params_wsc##:::##', '');
			process_elem=process_elem.replace('##:::##value##:::##', 'COMPANY');
		}
		vCardText+=process_elem;
	}

// PHOTO
	if(globalDisabledContactAttributes.indexOf('PHOTO')==-1 && !tmpvCardEditorRef.find('#photo').hasClass('photo_blank'))
	{
		var value = $('#photoURLHidden').val() || tmpvCardEditorRef.find('#photo').get(0).toDataURL('image/png');
		if(vCard.tplM['contentline_PHOTO']!=null && (process_elem=vCard.tplM['contentline_PHOTO'][0])!=undefined)
		{
			// replace the object and related objects' group names (+ append the related objects after the processed)
			parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
			if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
				process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+(groupCounter++)+'.').substring(2);

			process_elem=process_elem.replace('##:::##value##:::##',value);
		}
		else
		{
			process_elem=vCard.tplC['contentline_PHOTO'];
			process_elem=process_elem.replace('##:::##group_wd##:::##', '');
			process_elem=process_elem.replace('##:::##value##:::##', value);
		}

		// Data URL (non-remote) will always be a binary encoded png image
		if($('#photoURLHidden').val()==='') {
			process_elem=process_elem.replace('##:::##params_wsc##:::##', ';ENCODING=b;TYPE=png');
		}
		// For remote URL, we can't reliably determine its type, so we just append the VALUE=URI param
		else {
			process_elem=process_elem.replace('##:::##params_wsc##:::##', ';VALUE=URI');
		}

		vCardText+=process_elem;
	}

// ADR
	if(globalDisabledContactAttributes.indexOf('ADR')==-1)
	{
		tmpvCardEditorRef.find('[data-type="\\%address"]').each(
			function (index,element)
			{
				// if data is present for the selected country's address fields
				var found=0;
				$(element).find('[data-addr-field]').each(
					function(index,element)
					{
						if($(element).attr('data-addr-field')!='' && $(element).attr('data-addr-field')!='country' && $(element).val()!='')
						{
							found=1;
							return false;
						}
					}
				);
				if(found)
				{
					var incGroupCounter=false;
					if(vCard.tplM['contentline_ADR']!=null && (process_elem=vCard.tplM['contentline_ADR'][$(element).attr('data-id')])!=undefined)
					{
						// replace the object and related objects' group names (+ append the related objects after the processed)
						parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
						if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
						{
							process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+groupCounter+'.').substring(2);
							incGroupCounter=true;
						}
					}
					else
					{
						process_elem=vCard.tplC['contentline_ADR'];
						process_elem=process_elem.replace('##:::##group_wd##:::##', '');
					}

					tmp_type=$(element).find('[data-type="address_type"] option').filter(':selected').attr('data-type');

					/* construct the "custom" type */
					if(tmp_type==':custom')
					{
						var tmp_cust_value=$(element).find('[data-type="custom_value"]').val();
						var tmp_cust_value_processed=tmp_cust_value.replace(RegExp('^\\s*|\\s*$','g'),'').replaceAll('  ',' ');
						// if a custom type is already defined as standard type, use the standard definition
						if((tmp_cust_already_exists=$(element).find('[data-type="address_type"] option').filter(function(){return $(this).html()==tmp_cust_value_processed;}).attr('data-type'))!=undefined)
							tmp_type=tmp_cust_already_exists;
						else	// use custom type
							tmp_type=':'+tmp_cust_value+':';
					}

					params_wsc='';
					tmp_normal_types=tmp_type.replace(RegExp(':.*:','g'),',').replaceAll(',,',',').replace(RegExp('^,|,$','g'),'');
					if(tmp_normal_types!='')
						params_wsc=';TYPE='+vcardEscapeValue(tmp_normal_types).toUpperCase().replace(RegExp('\\\\,','g'),';TYPE=');

					var streetVal = $(element).find('[data-addr-field="street"]').map(function() {
						var val = $(this).val();

						if(val) {
							return val;
						}
					}).get().join('\n');

					process_elem=process_elem.replace('##:::##params_wsc##:::##',params_wsc);
					process_elem=process_elem.replace('##:::##pobox##:::##',vcardEscapeValue($(element).find('[data-addr-field="pobox"]').val()));
					process_elem=process_elem.replace('##:::##extaddr##:::##',vcardEscapeValue($(element).find('[data-addr-field="extaddr"]').val()));
					process_elem=process_elem.replace('##:::##street##:::##',vcardEscapeValue(streetVal));
					process_elem=process_elem.replace('##:::##locality##:::##',vcardEscapeValue($(element).find('[data-addr-field="locality"]').val()));
					process_elem=process_elem.replace('##:::##region##:::##',vcardEscapeValue($(element).find('[data-addr-field="region"]').val()));
					process_elem=process_elem.replace('##:::##code##:::##',vcardEscapeValue($(element).find('[data-addr-field="code"]').val()));
					process_elem=process_elem.replace('##:::##country##:::##',vcardEscapeValue($(element).find('[data-type="country_type"] option').filter(':selected').attr('data-full-name')));

					my_related='X-ABADR:'+vcardEscapeValue($(element).find('[data-type="country_type"] option').filter(':selected').attr('data-type'))+'\r\n';
					parsed=('\r\n'+process_elem).match(vCard.pre['contentline_parse']);
					if(parsed[1]!='')	// if group is present, we use it, otherwise we create a new group
						process_elem+=parsed[1]+my_related;
					else
						process_elem='item'+groupCounter+'.'+process_elem+'item'+groupCounter+'.'+my_related;
					incGroupCounter=true;	// we always increate the group number, because the X-ABADR is always stored

					my_related='';
					tmp_related_type=tmp_type.match(RegExp(':(.*):'));	// only one element of related (X-ABLabel) is supported

					if(tmp_related_type!=null && tmp_related_type[1]!='')
						my_related='X-ABLabel:'+vcardEscapeValue((dataTypes['address_type_store_as'][tmp_related_type[1]]!=undefined ? dataTypes['address_type_store_as'][tmp_related_type[1]] : tmp_related_type[1]))+'\r\n';

					if(my_related!='')
					{
						incGroupCounter=true;
						parsed=('\r\n'+process_elem).match(vCard.pre['contentline_parse']);
						if(parsed[1]!='')	// if group is present, we use it, otherwise we create a new group
							process_elem+=parsed[1]+my_related;
						else
							process_elem='item'+groupCounter+'.'+process_elem+'item'+groupCounter+'.'+my_related;
					}

					if(incGroupCounter) groupCounter++;
					vCardText+=process_elem;
				}
			}
		);
	}

// TEL
	if(globalDisabledContactAttributes.indexOf('TEL')==-1)
	{
		tmpvCardEditorRef.find('[data-type="\\%phone"]').each(
			function (index,element)
			{
				if((value=$(element).find('[data-type="value"]').val())!='')
				{
					var incGroupCounter=false;
					if(vCard.tplM['contentline_TEL']!=null && (process_elem=vCard.tplM['contentline_TEL'][$(element).attr('data-id')])!=undefined)
					{
						// replace the object and related objects' group names (+ append the related objects after the processed)
						parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
						if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
						{
							process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+groupCounter+'.').substring(2);
							incGroupCounter=true;
						}
					}
					else
					{
						process_elem=vCard.tplC['contentline_TEL'];
						process_elem=process_elem.replace('##:::##group_wd##:::##', '');
					}
					tmp_type=$(element).find('[data-type="phone_type"] option').filter(':selected').attr('data-type');

					/* construct the "custom" type */
					if(tmp_type==':custom')
					{
						var tmp_cust_value=$(element).find('[data-type="custom_value"]').val();
						var tmp_cust_value_processed=tmp_cust_value.replace(RegExp('^\\s*|\\s*$','g'),'').replaceAll('  ',' ');
						// if a custom type is already defined as standard type, use the standard definition
						if((tmp_cust_already_exists=$(element).find('[data-type="phone_type"] option').filter(function(){return $(this).html()==tmp_cust_value_processed;}).attr('data-type'))!=undefined)
							tmp_type=tmp_cust_already_exists;
						else	// use custom type
							tmp_type=':'+tmp_cust_value+':';
					}

					params_wsc='';
					tmp_normal_types=tmp_type.replace(RegExp(':.*:','g'),',').replaceAll(',,',',').replace(RegExp('^,|,$','g'),'');

					if(tmp_normal_types!='')
						params_wsc=';TYPE='+vcardEscapeValue(tmp_normal_types).toUpperCase().replace(RegExp('\\\\,','g'),';TYPE=');

					process_elem=process_elem.replace('##:::##params_wsc##:::##',params_wsc);
					process_elem=process_elem.replace('##:::##value##:::##',vcardEscapeValue(value));

					my_related='';
					tmp_related_type=tmp_type.match(RegExp(':(.*):'));	// only one element of related (X-ABLabel) is supported

					if(tmp_related_type!=null && tmp_related_type[1]!='')
						my_related='X-ABLabel:'+vcardEscapeValue((dataTypes['phone_type_store_as'][tmp_related_type[1]]!=undefined ? dataTypes['phone_type_store_as'][tmp_related_type[1]] : tmp_related_type[1]))+'\r\n';

					if(my_related!='')
					{
						incGroupCounter=true;
						parsed=('\r\n'+process_elem).match(vCard.pre['contentline_parse']);
						if(parsed[1]!='')	// if group is present, we use it, otherwise we create a new group
							process_elem+=parsed[1]+my_related;
						else
							process_elem='item'+groupCounter+'.'+process_elem+'item'+groupCounter+'.'+my_related;
					}

					if(incGroupCounter) groupCounter++;
					vCardText+=process_elem;
				}
			}
		);
	}

// EMAIL
	if(globalDisabledContactAttributes.indexOf('EMAIL')==-1)
	{
		tmpvCardEditorRef.find('[data-type="\\%email"]').each(
			function (index,element)
			{
				if((value=$(element).find('[data-type="value"]').val())!='')
				{
					incGroupCounter=false;
					if(vCard.tplM['contentline_EMAIL']!=null && (process_elem=vCard.tplM['contentline_EMAIL'][$(element).attr('data-id')])!=undefined)
					{
						// replace the object and related objects' group names (+ append the related objects after the processed)
						parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
						if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
						{
							process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+groupCounter+'.').substring(2);
							incGroupCounter=true;
						}
					}
					else
					{
						process_elem=vCard.tplC['contentline_EMAIL'];
						process_elem=process_elem.replace('##:::##group_wd##:::##', '');
					}

					tmp_type=$(element).find('[data-type="email_type"] option').filter(':selected').attr('data-type');

					/* construct the "custom" type */
					if(tmp_type==':custom')
					{
						var tmp_cust_value=$(element).find('[data-type="custom_value"]').val();
						var tmp_cust_value_processed=tmp_cust_value.replace(RegExp('^\\s*|\\s*$','g'),'').replaceAll('  ',' ');
						// if a custom type is already defined as standard type, use the standard definition
						if((tmp_cust_already_exists=$(element).find('[data-type="email_type"] option').filter(function(){return $(this).html()==tmp_cust_value_processed;}).attr('data-type'))!=undefined)
							tmp_type=tmp_cust_already_exists;
						else	// use custom type
							tmp_type=':'+tmp_cust_value+':';
					}

					params_wsc='';
					tmp_normal_types=tmp_type.replace(RegExp(':.*:','g'),',').replaceAll(',,',',').replace(RegExp('^,|,$','g'),'');
					if(tmp_normal_types!='')
						params_wsc=';TYPE='+vcardEscapeValue(tmp_normal_types).toUpperCase().replace(RegExp('\\\\,','g'),';TYPE=');

					process_elem=process_elem.replace('##:::##params_wsc##:::##',params_wsc);
					process_elem=process_elem.replace('##:::##value##:::##',vcardEscapeValue(value));

					my_related='';
					tmp_related_type=tmp_type.match(RegExp(':(.*):'));	// only one element of related (X-ABLabel) is supported

					if(tmp_related_type!=null && tmp_related_type[1]!='')
						my_related='X-ABLabel:'+vcardEscapeValue((dataTypes['email_type_store_as'][tmp_related_type[1]]!=undefined ? dataTypes['email_type_store_as'][tmp_related_type[1]] : tmp_related_type[1]))+'\r\n';

					if(my_related!='')
					{
						incGroupCounter=true;
						parsed=('\r\n'+process_elem).match(vCard.pre['contentline_parse']);
						if(parsed[1]!='')	// if group is present, we use it, otherwise we create a new group
							process_elem+=parsed[1]+my_related;
						else
							process_elem='item'+groupCounter+'.'+process_elem+'item'+groupCounter+'.'+my_related;
					}

					if(incGroupCounter) groupCounter++;
					vCardText+=process_elem;
				}
			}
		);
	}

// URL
	if(globalDisabledContactAttributes.indexOf('URL')==-1)
	{
		tmpvCardEditorRef.find('[data-type="\\%url"]').each(
			function (index,element)
			{
				if((value=$(element).find('[data-type="value"]').val())!='')
				{
					incGroupCounter=false;
					if(vCard.tplM['contentline_URL']!=null && (process_elem=vCard.tplM['contentline_URL'][$(element).attr('data-id')])!=undefined)
					{
						// replace the object and related objects' group names (+ append the related objects after the processed)
						parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
						if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
						{
							process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+groupCounter+'.').substring(2);
							incGroupCounter=true;
						}
					}
					else
					{
						process_elem=vCard.tplC['contentline_URL'];
						process_elem=process_elem.replace('##:::##group_wd##:::##', '');
					}

					tmp_type=$(element).find('[data-type="url_type"] option').filter(':selected').attr('data-type');

					/* construct the "custom" type */
					if(tmp_type==':custom')
					{
						var tmp_cust_value=$(element).find('[data-type="custom_value"]').val();
						var tmp_cust_value_processed=tmp_cust_value.replace(RegExp('^\\s*|\\s*$','g'),'').replaceAll('  ',' ');
						// if a custom type is already defined as standard type, use the standard definition
						if((tmp_cust_already_exists=$(element).find('[data-type="url_type"] option').filter(function(){return $(this).html()==tmp_cust_value_processed;}).attr('data-type'))!=undefined)
							tmp_type=tmp_cust_already_exists;
						else	// use custom type
							tmp_type=':'+tmp_cust_value+':';
					}

					params_wsc='';
					tmp_normal_types=tmp_type.replace(RegExp(':.*:','g'),',').replaceAll(',,',',').replace(RegExp('^,|,$','g'),'');
					if(tmp_normal_types!='')
						params_wsc=';TYPE='+vcardEscapeValue(tmp_normal_types).toUpperCase().replace(RegExp('\\\\,','g'),';TYPE=');

					process_elem=process_elem.replace('##:::##params_wsc##:::##',params_wsc);
					process_elem=process_elem.replace('##:::##value##:::##',vcardEscapeValue(value));

					my_related='';
					tmp_related_type=tmp_type.match(RegExp(':(.*):'));	// only one element of related (X-ABLabel) is supported

					if(tmp_related_type!=null && tmp_related_type[1]!='')
						my_related='X-ABLabel:'+vcardEscapeValue((dataTypes['url_type_store_as'][tmp_related_type[1]]!=undefined ? dataTypes['url_type_store_as'][tmp_related_type[1]] : tmp_related_type[1]))+'\r\n';

					if(my_related!='')
					{
						incGroupCounter=true;
						parsed=('\r\n'+process_elem).match(vCard.pre['contentline_parse']);
						if(parsed[1]!='')	// if group is present, we use it, otherwise we create a new group
							process_elem+=parsed[1]+my_related;
						else
							process_elem='item'+groupCounter+'.'+process_elem+'item'+groupCounter+'.'+my_related;
					}

					if(incGroupCounter) groupCounter++;
					vCardText+=process_elem;
				}
			}
		);
	}

// X-ABRELATEDNAMES
	if(globalDisabledContactAttributes.indexOf('X-ABRELATEDNAMES')==-1)
	{
		tmpvCardEditorRef.find('[data-type="\\%person"]').each(
			function (index,element)
			{
				if((value=$(element).find('[data-type="value"]').val())!='')
				{
					incGroupCounter=false;
					if(vCard.tplM['contentline_X-ABRELATEDNAMES']!=null && (process_elem=vCard.tplM['contentline_X-ABRELATEDNAMES'][$(element).attr('data-id')])!=undefined)
					{
						// replace the object and related objects' group names (+ append the related objects after the processed)
						parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
						if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
						{
							process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+groupCounter+'.').substring(2);
							incGroupCounter=true;
						}
					}
					else
					{
						process_elem=vCard.tplC['contentline_X-ABRELATEDNAMES'];
						process_elem=process_elem.replace('##:::##group_wd##:::##', '');
					}

					tmp_type=$(element).find('[data-type="person_type"] option').filter(':selected').attr('data-type');

					/* construct the "custom" type */
					if(tmp_type==':custom')
					{
						var tmp_cust_value=$(element).find('[data-type="custom_value"]').val();
						var tmp_cust_value_processed=tmp_cust_value.replace(RegExp('^\\s*|\\s*$','g'),'').replaceAll('  ',' ');
						// if a custom type is already defined as standard type, use the standard definition
						if((tmp_cust_already_exists=$(element).find('[data-type="person_type"] option').filter(function(){return $(this).html()==tmp_cust_value_processed;}).attr('data-type'))!=undefined)
							tmp_type=tmp_cust_already_exists;
						else	// use custom type
							tmp_type=':'+tmp_cust_value+':';
					}

					params_wsc='';
					tmp_normal_types=tmp_type.replace(RegExp(':.*:','g'),',').replaceAll(',,',',').replace(RegExp('^,|,$','g'),'');
					if(tmp_normal_types!='')
						params_wsc=';TYPE='+vcardEscapeValue(tmp_normal_types).toUpperCase().replace(RegExp('\\\\,','g'),';TYPE=');

					process_elem=process_elem.replace('##:::##params_wsc##:::##',params_wsc);
					process_elem=process_elem.replace('##:::##value##:::##',vcardEscapeValue(value));

					my_related='';
					tmp_related_type=tmp_type.match(RegExp(':(.*):'));	// only one element of related (X-ABLabel) is supported

					if(tmp_related_type!=null && tmp_related_type[1]!='')
						my_related='X-ABLabel:'+vcardEscapeValue((dataTypes['person_type_store_as'][tmp_related_type[1]]!=undefined ? dataTypes['person_type_store_as'][tmp_related_type[1]] : tmp_related_type[1]))+'\r\n';

					if(my_related!='')
					{
						incGroupCounter=true;
						parsed=('\r\n'+process_elem).match(vCard.pre['contentline_parse']);
						if(parsed[1]!='')	// if group is present, we use it, otherwise we create a new group
							process_elem+=parsed[1]+my_related;
						else
							process_elem='item'+groupCounter+'.'+process_elem+'item'+groupCounter+'.'+my_related;
					}

					if(incGroupCounter) groupCounter++;

					if(tmp_related_type!=null && tmp_related_type[1]!='')
					{
						// In addition of the X-ABRELATEDNAMES attributes add also the old style X-* attributes
						switch(tmp_related_type[1])
						{
							case '_$!<assistant>!$_':
								process_elem+='X-ASSISTANT:'+vcardEscapeValue(value)+'\r\n';
								// process_elem+='X-EVOLUTION-ASSISTANT:'+vcardEscapeValue(value)+'\r\n';
								break;
							case '_$!<manager>!$_':
								process_elem+='X-MANAGER:'+vcardEscapeValue(value)+'\r\n';
								// process_elem+='X-EVOLUTION-MANAGER:'+vcardEscapeValue(value)+'\r\n';
								break;
							case '_$!<spouse>!$_':
								process_elem+='X-SPOUSE:'+vcardEscapeValue(value)+'\r\n';
								// process_elem+='X-EVOLUTION-SPOUSE:'+vcardEscapeValue(value)+'\r\n';
								break;
							default:
								break;
						}
					}

					vCardText+=process_elem;
				}
			}
		);
	}

// IMPP
	if(globalDisabledContactAttributes.indexOf('IMPP')==-1)
	{
		tmpvCardEditorRef.find('[data-type="\\%im"]').each(
			function (index,element)
			{
				if((value=$(element).find('[data-type="value"]').val())!='')
				{
					incGroupCounter=false;
					if(vCard.tplM['contentline_IMPP']!=null && (process_elem=vCard.tplM['contentline_IMPP'][$(element).attr('data-id')])!=undefined)
					{
						// replace the object and related objects' group names (+ append the related objects after the processed)
						parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
						if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
						{
							process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+groupCounter+'.').substring(2);
							incGroupCounter=true;
						}
					}
					else
					{
						process_elem=vCard.tplC['contentline_IMPP'];
						process_elem=process_elem.replace('##:::##group_wd##:::##', '');
					}

					tmp_type=$(element).find('[data-type="im_type"] option').filter(':selected').attr('data-type');

					/* construct the "custom" type */
					if(tmp_type==':custom')
					{
						var tmp_cust_value=$(element).find('[data-type="custom_value"]:first').val();
						var tmp_cust_value_processed=tmp_cust_value.replace(RegExp('^\\s*|\\s*$','g'),'').replaceAll('  ',' ');
						// if a custom type is already defined as standard type, use the standard definition
						if((tmp_cust_already_exists=$(element).find('[data-type="im_type"] option').filter(function(){return $(this).html()==tmp_cust_value_processed;}).attr('data-type'))!=undefined)
							tmp_type=tmp_cust_already_exists;
						else	// use custom type
							tmp_type=':'+tmp_cust_value+':';
					}

					params_wsc=params_wsc_old_repr='';
					tmp_normal_types=tmp_type.replace(RegExp(':.*:','g'),',').replaceAll(',,',',').replace(RegExp('^,|,$','g'),'');
					if(tmp_normal_types!='')
						params_wsc=params_wsc_old_repr=';TYPE='+vcardEscapeValue(tmp_normal_types).toUpperCase().replace(RegExp('\\\\,','g'),';TYPE=');

					tmp_service_type=$(element).find('[data-type="im_service_type"] option').filter(':selected').attr('data-type');

					/* construct the "custom" type */
					if(tmp_service_type==':custom')
					{
						var tmp_cust_value=$(element).find('[data-type="custom_value"]:last').val();
						var tmp_cust_value_processed=tmp_cust_value.replace(RegExp('^\\s*|\\s*$','g'),'').replaceAll('  ',' ');
						// if a custom type is already defined as standard type, use the standard definition
						if((tmp_cust_already_exists=$(element).find('[data-type="im_service_type"] option').filter(function(){return $(this).html()==tmp_cust_value_processed;}).attr('data-type'))!=undefined)
							tmp_service_type=tmp_cust_already_exists;
						else	// use custom type
							tmp_service_type=':'+tmp_cust_value+':';
					}

					if(dataTypes['im_service_type_store_as'][tmp_service_type]!=undefined)
						tmp_service_type=dataTypes['im_service_type_store_as'][tmp_service_type];
					params_wsc=';X-SERVICE-TYPE='+vcardEscapeValue(tmp_service_type)+params_wsc;

					process_elem=process_elem.replace('##:::##params_wsc##:::##',params_wsc);
					switch(tmp_service_type.toLowerCase())	// RFC4770
					{
						case 'aim':
							im_value='aim:'+vcardEscapeValue(value);
							break;
						case 'facebook':
							im_value='xmpp:'+vcardEscapeValue(value);
							break;
						case 'googletalk':
							im_value='xmpp:'+vcardEscapeValue(value);
							break;
						case 'icq':
							im_value='aim:'+vcardEscapeValue(value);
							break;
						case 'irc':
							im_value='irc:'+vcardEscapeValue(value);
							break;
						case 'jabber':
							im_value='xmpp:'+vcardEscapeValue(value);
							break;
						case 'msn':
							im_value='msnim:'+vcardEscapeValue(value);
							break;
						case 'skype':
							im_value='skype:'+vcardEscapeValue(value);
							break;
						case 'yahoo':
							im_value='ymsgr:'+vcardEscapeValue(value);
							break;
						default:	// 'gadugadu', 'qq', ...
							im_value='x-apple:'+vcardEscapeValue(value);
							break;
					}
					process_elem=process_elem.replace('##:::##value##:::##',im_value);

					my_related='';
					tmp_related_type=tmp_type.match(RegExp(':(.*):'));	// only one element of related (X-ABLabel) is supported

					if(tmp_related_type!=null && tmp_related_type[1]!='')
						my_related='X-ABLabel:'+vcardEscapeValue((dataTypes['im_type_store_as'][tmp_related_type[1]]!=undefined ? dataTypes['im_type_store_as'][tmp_related_type[1]] : tmp_related_type[1]))+'\r\n';

					if(my_related!='')
					{
						incGroupCounter=true;
						parsed=('\r\n'+process_elem).match(vCard.pre['contentline_parse']);
						if(parsed[1]!='')	// if group is present, we use it, otherwise we create a new group
							process_elem+=parsed[1]+my_related;
						else
							process_elem='item'+groupCounter+'.'+process_elem+'item'+groupCounter+'.'+my_related;
					}
					if(incGroupCounter) groupCounter++;

					// In addition of the IMPP attributes add also the old style X-* attributes
					process_elem_old_repr='';
					switch(tmp_service_type.toLowerCase())
					{
						case 'aim':
							new_group_wd='';
							if(incGroupCounter)
							{
								new_group_wd='item'+groupCounter+'.';
								process_elem_old_repr=('\r\n'+process_elem).replace(RegExp('\r\nitem'+(groupCounter-1)+'\\.','mg'),'\r\n'+new_group_wd);
								groupCounter++;
							}
							else
								process_elem_old_repr='\r\n'+process_elem;
							process_elem+=process_elem_old_repr.replace('\r\n'+new_group_wd+'IMPP;X-SERVICE-TYPE='+ vcardEscapeValue(tmp_service_type),new_group_wd+'X-AIM').replace(im_value+'\r\n',vcardEscapeValue(value)+'\r\n');
							break;
						case 'jabber':
							new_group_wd='';
							if(incGroupCounter)
							{
								new_group_wd='item'+groupCounter+'.';
								process_elem_old_repr=('\r\n'+process_elem).replace(RegExp('\r\nitem'+(groupCounter-1)+'\\.','mg'),'\r\n'+new_group_wd);
								groupCounter++;
							}
							else
								process_elem_old_repr='\r\n'+process_elem;
							process_elem+=process_elem_old_repr.replace('\r\n'+new_group_wd+'IMPP;X-SERVICE-TYPE='+ vcardEscapeValue(tmp_service_type),new_group_wd+'X-JABBER').replace(im_value+'\r\n',vcardEscapeValue(value)+'\r\n');
							break;
						case 'msn':
							new_group_wd='';
							if(incGroupCounter)
							{
								new_group_wd='item'+groupCounter+'.';
								process_elem_old_repr=('\r\n'+process_elem).replace(RegExp('\r\nitem'+(groupCounter-1)+'\\.','mg'),'\r\n'+new_group_wd);
								groupCounter++;
							}
							else
								process_elem_old_repr='\r\n'+process_elem;
							process_elem+=process_elem_old_repr.replace('\r\n'+new_group_wd+'IMPP;X-SERVICE-TYPE='+ vcardEscapeValue(tmp_service_type),new_group_wd+'X-MSN').replace(im_value+'\r\n',vcardEscapeValue(value)+'\r\n');
							break;
						case 'yahoo':
							new_group_wd='';
							process_elem_tmp=process_elem;
							if(incGroupCounter)
							{
								new_group_wd='item'+groupCounter+'.';
								process_elem_old_repr=('\r\n'+process_elem_tmp).replace(RegExp('\r\nitem'+(groupCounter-1)+'\\.','mg'),'\r\n'+new_group_wd);
								groupCounter++;
							}
							else
								process_elem_old_repr='\r\n'+process_elem;
							process_elem+=process_elem_old_repr.replace('\r\n'+new_group_wd+'IMPP;X-SERVICE-TYPE='+ vcardEscapeValue(tmp_service_type),new_group_wd+'X-YAHOO').replace(im_value+'\r\n',vcardEscapeValue(value)+'\r\n');

							new_group_wd='';
							if(incGroupCounter)
							{
								new_group_wd='item'+groupCounter+'.';
								process_elem_old_repr=('\r\n'+process_elem_tmp).replace(RegExp('\r\nitem'+(groupCounter-2)+'\\.','mg'),'\r\n'+new_group_wd);
								groupCounter++;
							}
							else
								process_elem_old_repr='\r\n'+process_elem;
							process_elem+=process_elem_old_repr.replace('\r\n'+new_group_wd+'IMPP;X-SERVICE-TYPE='+ vcardEscapeValue(tmp_service_type),new_group_wd+'X-YAHOO-ID').replace(im_value+'\r\n',vcardEscapeValue(value)+'\r\n');
							break;
						case 'icq':
							new_group_wd='';
							if(incGroupCounter)
							{
								new_group_wd='item'+groupCounter+'.';
								process_elem_old_repr=('\r\n'+process_elem).replace(RegExp('\r\nitem'+(groupCounter-1)+'\\.','mg'),'\r\n'+new_group_wd);
								groupCounter++;
							}
							else
								process_elem_old_repr='\r\n'+process_elem;
							process_elem+=process_elem_old_repr.replace('\r\n'+new_group_wd+'IMPP;X-SERVICE-TYPE='+ vcardEscapeValue(tmp_service_type),new_group_wd+'X-ICQ').replace(im_value+'\r\n',vcardEscapeValue(value)+'\r\n');
							break;
						default:
							break;
					}
					vCardText+=process_elem;
				}
			}
		);
	}

// X-SOCIALPROFILE
	if(globalDisabledContactAttributes.indexOf('X-SOCIALPROFILE')==-1)
	{
		tmpvCardEditorRef.find('[data-type="\\%profile"]').each(
			function (index,element)
			{
				if((value=$(element).find('[data-type="value"]').val())!='')
				{
					incGroupCounter=false;
					if(vCard.tplM['contentline_X-SOCIALPROFILE']!=null && (process_elem=vCard.tplM['contentline_X-SOCIALPROFILE'][$(element).attr('data-id')])!=undefined)
					{
						// replace the object and related objects' group names (+ append the related objects after the processed)
						parsed=('\r\n'+process_elem).match(RegExp('\r\n((?:'+vCard.re['group']+'\\.)?)','m'));
						if(parsed[1]!='')	// if group is present, replace the object and related objects' group names
						{
							process_elem=('\r\n'+process_elem).replace(RegExp('\r\n'+parsed[1].replace('.','\\.'),'mg'),'\r\nitem'+groupCounter+'.').substring(2);
							incGroupCounter=true;
						}
					}
					else
					{
						process_elem=vCard.tplC['contentline_X-SOCIALPROFILE'];
						process_elem=process_elem.replace('##:::##group_wd##:::##', '');
					}

					tmp_type=$(element).find('[data-type="profile_type"] option').filter(':selected').attr('data-type');

					/* construct the "custom" type */
					if(tmp_type==':custom')
					{
						var tmp_cust_value=$(element).find('[data-type="custom_value"]').val();
						var tmp_cust_value_processed=tmp_cust_value.replace(RegExp('^\\s*|\\s*$','g'),'').replaceAll('  ',' ');
						// if a custom type is already defined as standard type, use the standard definition
						if((tmp_cust_already_exists=$(element).find('[data-type="profile_type"] option').filter(function(){return $(this).html()==tmp_cust_value_processed;}).attr('data-type'))!=undefined)
							tmp_type=tmp_cust_already_exists;
						else	// use custom type
							tmp_type=':'+tmp_cust_value+':';
					}

					params_wsc='';
					tmp_normal_types=tmp_type.replace(RegExp(':.*:','g'),',').replaceAll(',,',',').replace(RegExp('^,|,$','g'),'');
					if(tmp_normal_types!='')
						params_wsc=';TYPE='+vcardEscapeValue(tmp_normal_types).toUpperCase().replace(RegExp('\\\\,','g'),';TYPE=')+';x-user='+vcardEscapeValue(tmp_type=='twitter' ? value.replace(/^@+/, '') : value);

					process_elem=process_elem.replace('##:::##params_wsc##:::##',params_wsc);
					process_elem=process_elem.replace('##:::##value##:::##', vcardEscapeValue((globalSettings.urihandlerprofile.value[tmp_type]!=undefined ? globalSettings.urihandlerprofile.value[tmp_type] : 'x-apple:%u').replace('%u', (tmp_type=='twitter' ? value.replace(/^@+/, '') : value))));

					if(incGroupCounter) groupCounter++;
					vCardText+=process_elem;
				}
			}
		);
	}

	// extension hook
	if(typeof(globalContactsExtDataToVcard)=='function')
		vCardText=globalContactsExtDataToVcard(tmpvCardEditorRef, vCardText);

	// PRODID
	vCardText+='PRODID:-//Inf-IT//'+globalAppName+' '+globalVersion+'//EN\r\n';

	if(typeof vCard.tplM['unprocessed_unrelated']!='undefined')
		vCardText+=vCard.tplM['unprocessed_unrelated'].replace(RegExp('^\r\n'),'');

	// vCard END (required by RFC)
	if(vCard.tplM['end']!=null && (process_elem=vCard.tplM['end'][0])!=undefined)
		vCardText+=vCard.tplM['end'][0];
	else
	{
		process_elem=vCard.tplC['end'];
		process_elem=process_elem.replace('##:::##group_wd##:::##', '');
		vCardText+=process_elem;
	}

	// replace unsupported XML characters
	vCardText=vCardText.replace(/[^\u0009\u000A\u000D\u0020-\uD7FF\uE000-\uFFFD]/g, ' ');

	// line folding (RFC2426 - section 2.6) - maximum of 75 octects (and cannot break
	//  multi-octet UTF8-characters) allowed on one line, excluding a line break (CRLF)
	vCardText=vObjectLineFolding(vCardText);

	if(typeof(globalContactsExtPutVcardToCollectionOverload)=='function')
		globalContactsExtPutVcardToCollectionOverload(accountUID, inputEtag, newUID, vCardText);
	else
	{
		var selAddr = tmpvCardEditorRef.find('[data-attr-name="_DEST_"]').find('option:selected').attr('data-type')
		//addressbook selectbox was changed
		var orgAddr = $('#vCardEditor').attr('data-url').replace(RegExp('[^/]*$'),'');
		if($('#ExtendedDest').length>0)
		{
			var putGroups=new Array();
			var removeGroups=new Array();
			var myGroups = new Array()
			if(inputEtag!='')
			{
				myGroups=globalAddressbookList.getMyContactGroups($('#vCardEditor').attr('data-url'));
				for(var gi=0; gi<myGroups.length; gi++)
					if($('#ExtendedDest').find('.extended_dest_group').find('input:checked[data-id="'+myGroups[gi]+'"]').length==0)
						removeGroups.push(myGroups[gi]);
			}
			$('#ExtendedDest').find('.extended_dest_group').find('input:checked').each(function(){
				var guid = $(this).attr('data-id');
				if(myGroups.indexOf(guid)==-1)
					putGroups.push(guid);
			});
		}
		if(orgAddr!= selAddr && inputEtag!='')
		{
			var tmp2=globalAddressbookList.getContactByUID($('#vCardEditor').attr('data-url'));
			var vUID = $('#vCardEditor').attr('data-url').match(RegExp('[^/]*$'));
			// here we generate the destination for MOVE (we don't use the old vCard file name to minimalize the possible conflict situations)
			tmp2.vcard=vCardText;
			tmp2.newAccountUID=globalResourceCardDAVList.getCollectionByUID(selAddr).accountUID;
			tmp2.newUid=selAddr;
			tmp2.finalContactUID=tmp2.uid;
			tmp2.orgUID=selAddr+vUID;
			tmp2.addToContactGroupUID=new Array();
			tmp2.removeToContactGroupUID=new Array();
			// we need to store the ui object references for error handling in the GUI
			if($('#ExtendedDest').length>0)
			{
				tmp2.uiObjects={resource:globalRefAddContact.attr('data-filter-url')};
				if(putGroups.length>0)
					tmp2.addToContactGroupUID=putGroups.slice();
				if(removeGroups.length>0)
					tmp2.removeToContactGroupUID=removeGroups.slice();
			}
			tmp2.formSave=true;
			lockAndPerformToCollection(tmp2, globalRefAddContact.attr('data-filter-url'), 'IRM_DELETE');
		}
		else
		{
			if(inputEtag=='')
				inputUID=selAddr;
			if($('#ExtendedDest').length>0 && (putGroups.length>0 || removeGroups.length>0))
			{
				if(inputEtag!='')
					var tmp2=globalAddressbookList.getContactByUID($('#vCardEditor').attr('data-url'));
				else
					var tmp2={accountUID: accountUID, uid: inputUID, etag: inputEtag};
				var vUID = $('#vCardEditor').attr('data-url').match(RegExp('[^/]*$'));
				// here we generate the destination for MOVE (we don't use the old vCard file name to minimalize the possible conflict situations)
				tmp2.vcard=vCardText;
				tmp2.uiObjects={resource:globalRefAddContact.attr('data-filter-url')};
				tmp2.addToContactGroupUID=new Array();
				tmp2.removeToContactGroupUID=new Array();
				if(putGroups.length>0)
					tmp2.addToContactGroupUID=putGroups.slice();
				if(removeGroups.length>0)
					tmp2.removeToContactGroupUID=removeGroups.slice();
				tmp2.formSave=true;
				lockAndPerformToCollection(tmp2, globalRefAddContact.attr('data-filter-url'), 'PUT');
			}
			else
				putVcardToCollectionMain({accountUID: accountUID, uid: inputUID, etag: inputEtag, vcard: vCardText}, inputFilterUID);
		}
	}
}

function vcardToData(inputContact, inputIsReadonly, inputIsCompany, inputEditorMode)
{
	if(typeof globalDebug!='undefined' && globalDebug instanceof Array && globalDebug.indexOf('vcard')!=-1)
		console.time('vcardToData timer');

	if(inputContact.vcard==undefined)
	{
		console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
		return false;
	}

	var tmpvCardEditorRef=CardDAVeditor_cleanup(false, inputIsCompany);	// editor initialization

	$('#ABContactColor').css('background-color', inputContact.color);

	if(typeof globalDisabledContactAttributes=='undefined' || !(globalDisabledContactAttributes instanceof Array))
		globalDisabledContactAttributes=[];

	if(inputContact.vcard.match(vCard.pre['vcard']))
	{
		// ------------------------------------------------------------------------------------- //
		// BEGIN and END
		vcard_full=inputContact.vcard.split('\r\n');		// vCard data to array

		// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
		if((parsed=('\r\n'+vcard_full[0]+'\r\n').match(vCard.pre['contentline_parse']))==null)
		{
			console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
			return false;
		}
		// values not directly supported by the editor (old values are kept intact)
		vCard.tplM['begin'][0]=vCard.tplC['begin'].replace('##:::##group_wd##:::##', vcard_begin_group=parsed[1]);
		// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
		if((parsed=('\r\n'+vcard_full[vcard_full.length-2]+'\r\n').match(vCard.pre['contentline_parse']))==null)
		{
			console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
			return false;
		}
		// values not directly supported by the editor (old values are kept intact)
		vCard.tplM['end'][0]=vCard.tplC['end'].replace('##:::##group_wd##:::##', vcard_end_group=parsed[1]);

		if(vcard_begin_group!=vcard_end_group)
		{
			console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
			return false;	// the vCard BEGIN and END "group" are different
		}

		// remove the vCard BEGIN and END
		vcard='\r\n'+vcard_full.slice(1, vcard_full.length-2).join('\r\n')+'\r\n';

//console.time('VERSION timer');
		// ------------------------------------------------------------------------------------- //
		// VERSION -> what to do if present more than once?
		vcard_element=vcard.match(vCard.pre['contentline_VERSION']);
		if(vcard_element!=null && vcard_element.length==1)	// if the VERSION attribute is not present exactly once, vCard is considered invalid
		{
			// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
			parsed=vcard_element[0].match(vCard.pre['contentline_parse']);
			if(parsed[3]=='' && parsed[4]=='3.0')	// RFC requirement
			{
				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_VERSION'][0]=vCard.tplC['contentline_VERSION'];
				vCard.tplM['contentline_VERSION'][0]=vCard.tplM['contentline_VERSION'][0].replace('##:::##group_wd##:::##', parsed[1]);
				vCard.tplM['contentline_VERSION'][0]=vCard.tplM['contentline_VERSION'][0].replace('##:::##version##:::##', parsed[4]);

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0], '\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_VERSION'][0]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0],'\r\n');
					}
				}
			}
			else
			{
				console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
				return false;	// invalid input for "VERSION" (we support only vCard 3.0)
			}
		}
		else
		{
			console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
			return false;	// vcard "VERSION" not present or present more than once
		}
//console.timeEnd('VERSION timer');

//console.time('UID timer');
		// ------------------------------------------------------------------------------------- //
		// UID -> TODO: what to do if present more than once?
		vcard_element=vcard.match(vCard.pre['contentline_UID']);
		if(vcard_element!=null && vcard_element.length==1)	// if the UID attribute is not present exactly once, vCard is considered invalid
		{
			// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
			parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

			// values not directly supported by the editor (old values are kept intact)
			vCard.tplM['contentline_UID'][0]=vCard.tplC['contentline_UID'];
			vCard.tplM['contentline_UID'][0]=vCard.tplM['contentline_UID'][0].replace('##:::##group_wd##:::##', parsed[1]);
			vCard.tplM['contentline_UID'][0]=vCard.tplM['contentline_UID'][0].replace('##:::##params_wsc##:::##', parsed[3]);
			vCard.tplM['contentline_UID'][0]=vCard.tplM['contentline_UID'][0].replace('##:::##uid##:::##', parsed[4]);

			tmpvCardEditorRef.find('#vCardEditor').attr('data-vcard-uid', parsed[4]);	// special hack; usually used by extension hooks

			// remove the processed parameter
			vcard=vcard.replace(vcard_element[0], '\r\n');

			// find the corresponding group data (if exists)
			if(parsed[1]!='')
			{
				var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
				while((vcard_element_related=vcard.match(re))!=null)
				{
					// append the parameter to its parent
					vCard.tplM['contentline_UID'][0]+=vcard_element_related[0].substr(2);
					// remove the processed parameter
					vcard=vcard.replace(vcard_element_related[0],'\r\n');
				}
			}
		}
// Old not RFC vCards not contain UID - we ignore this error (UID is generated if vCard is changed)
//		else
//		{
//			console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
//			return false;	// vcard UID not present or present more than once
//		}
//console.timeEnd('UID timer');

//console.time('FN timer');
		// ------------------------------------------------------------------------------------- //
		// FN -> TODO: what to do if present more than once?
		vcard_element=vcard.match(vCard.pre['contentline_FN']);
		if(vcard_element!=null && vcard_element.length==1)	// if the FN attribute is not present exactly once, vCard is considered invalid
		{
			// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
			parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

			// values not directly supported by the editor (old values are kept intact)
			vCard.tplM['contentline_FN'][0]=vCard.tplC['contentline_FN'];
			vCard.tplM['contentline_FN'][0]=vCard.tplM['contentline_FN'][0].replace('##:::##group_wd##:::##', parsed[1]);
			vCard.tplM['contentline_FN'][0]=vCard.tplM['contentline_FN'][0].replace('##:::##params_wsc##:::##', parsed[3]);

			// remove the processed parameter
			vcard=vcard.replace(vcard_element[0],'\r\n');

			// find the corresponding group data (if exists)
			if(parsed[1]!='')
			{
				var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
				while((vcard_element_related=vcard.match(re))!=null)
				{
					// append the parameter to its parent
					vCard.tplM['contentline_FN'][0]+=vcard_element_related[0].substr(2);
					// remove the processed parameter
					vcard=vcard.replace(vcard_element_related[0],'\r\n');
				}
			}
		}
		else
		{
			console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
			return false;	// vcard FN not present or present more than once
		}
//console.timeEnd('FN timer');

//console.time('N timer');
		// ------------------------------------------------------------------------------------- //
		// N -> TODO: what to do if present more than once?
		vcard_element=vcard.match(vCard.pre['contentline_N']);
		if(vcard_element!=null && vcard_element.length==1)	// if the N attribute is not present exactly once, vCard is considered invalid
		{
			// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
			parsed=vcard_element[0].match(vCard.pre['contentline_parse']);
			// parsed_value = [0]->Family, [1]->Given, [2]->Middle, [3]->Prefix, [4]->Suffix
			parsed_value=vcardSplitValue(parsed[4],';');

			if(parsed_value[0]!=undefined && parsed_value[0]!='')
				tmpvCardEditorRef.find('[data-type="family"]').val(vcardUnescapeValue(parsed_value[0]));
			if(parsed_value[1]!=undefined && parsed_value[1]!='')
				tmpvCardEditorRef.find('[data-type="given"]').val(vcardUnescapeValue(parsed_value[1]));
			if(parsed_value[2]!=undefined && parsed_value[2]!='')
				tmpvCardEditorRef.find('[data-type="middle"]').val(vcardUnescapeValue(parsed_value[2]));
			if(parsed_value[3]!=undefined && parsed_value[3]!='')
				tmpvCardEditorRef.find('[data-type="prefix"]').val(vcardUnescapeValue(parsed_value[3]));
			if(parsed_value[4]!=undefined && parsed_value[4]!='')
				tmpvCardEditorRef.find('[data-type="suffix"]').val(vcardUnescapeValue(parsed_value[4]));

			// values not directly supported by the editor (old values are kept intact)
			vCard.tplM['contentline_N'][0]=vCard.tplC['contentline_N'];
			vCard.tplM['contentline_N'][0]=vCard.tplM['contentline_N'][0].replace('##:::##group_wd##:::##', parsed[1]);
			vCard.tplM['contentline_N'][0]=vCard.tplM['contentline_N'][0].replace('##:::##params_wsc##:::##', parsed[3]);

			// remove the processed parameter
			vcard=vcard.replace(vcard_element[0],'\r\n');

			// find the corresponding group data (if exists)
			if(parsed[1]!='')
			{
				var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
				while((vcard_element_related=vcard.match(re))!=null)
				{
					// append the parameter to its parent
					vCard.tplM['contentline_N'][0]+=vcard_element_related[0].substr(2);
					// remove the processed parameter
					vcard=vcard.replace(vcard_element_related[0],'\r\n');
				}
			}
		}
		else
		{
			console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
			return false;	// vcard N not present or present more than once
		}
//console.timeEnd('N timer');

//console.time('CATEGORIES timer');
		// ------------------------------------------------------------------------------------- //
		// CATEGORIES -> present max. once because of the CardDavMATE vCard transformations
		if(globalDisabledContactAttributes.indexOf('CATEGORIES')==-1)
		{
			vcard_element=vcard.match(vCard.pre['contentline_CATEGORIES']);
			if(vcard_element!=null && vcard_element.length==1)
			{
				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

				tmpvCardEditorRef.find('#tags').importTags(parsed[4]);	// we do not need to unescape the value here!

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_CATEGORIES'][0]=vCard.tplC['contentline_CATEGORIES'];
				vCard.tplM['contentline_CATEGORIES'][0]=vCard.tplM['contentline_CATEGORIES'][0].replace('##:::##group_wd##:::##', parsed[1]);
				vCard.tplM['contentline_CATEGORIES'][0]=vCard.tplM['contentline_CATEGORIES'][0].replace('##:::##params_wsc##:::##', parsed[3]);

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0],'\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_CATEGORIES'][0]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0],'\r\n');
					}
				}
			}
		}
//console.timeEnd('CATEGORIES timer');

//console.time('NOTE timer');
		// ------------------------------------------------------------------------------------- //
		// NOTE -> TODO: what to do if present more than once?
		if(globalDisabledContactAttributes.indexOf('NOTE')==-1)
		{
			vcard_element=vcard.match(vCard.pre['contentline_NOTE']);
			if(vcard_element!=null)
			{
				if(vcard_element.length==1)	// if the NOTE attribute is present exactly once
				{
					// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
					parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

					tmpvCardEditorRef.find('[data-type="\\%note"]').find('textarea').text(vcardUnescapeValue(parsed[4])).trigger('autosize.resize');

					// values not directly supported by the editor (old values are kept intact)
					vCard.tplM['contentline_NOTE'][0]=vCard.tplC['contentline_NOTE'];
					vCard.tplM['contentline_NOTE'][0]=vCard.tplM['contentline_NOTE'][0].replace('##:::##group_wd##:::##', parsed[1]);
					vCard.tplM['contentline_NOTE'][0]=vCard.tplM['contentline_NOTE'][0].replace('##:::##params_wsc##:::##', parsed[3]);

					// remove the processed parameter
					vcard=vcard.replace(vcard_element[0],'\r\n');

					// find the corresponding group data (if exists)
					if(parsed[1]!='')
					{
						var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
						while((vcard_element_related=vcard.match(re))!=null)
						{
							// append the parameter to its parent
							vCard.tplM['contentline_NOTE'][0]+=vcard_element_related[0].substr(2);
							// remove the processed parameter
							vcard=vcard.replace(vcard_element_related[0],'\r\n');
						}
					}
				}
				else
				{
					console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
					return false;	// vcard NOTE present more than once
				}
			}
		}
//console.timeEnd('NOTE timer');

//console.time('REV timer');
		// ------------------------------------------------------------------------------------- //
		// REV -> what to do if present more than once?
		vcard_element=vcard.match(vCard.pre['contentline_REV']);
		if(vcard_element!=null)	// if the REV attribute is exists
		{
			if(vcard_element.length==1)	// and is present exactly once
			{
				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_REV'][0]=vCard.tplC['contentline_REV'];
				vCard.tplM['contentline_REV'][0]=vCard.tplM['contentline_REV'][0].replace('##:::##group_wd##:::##', parsed[1]);

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0],'\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_REV'][0]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0],'\r\n');
					}
				}
			}
			else
			{
				console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
				return false;	// vcard REV present more than once
			}
		}
//console.timeEnd('REV timer');

//console.time('NICKNAME timer');
		// ------------------------------------------------------------------------------------- //
		// NICKNAME -> TODO: what to do if present more than once?
		if(globalDisabledContactAttributes.indexOf('NICKNAME')==-1)
		{
			vcard_element=vcard.match(vCard.pre['contentline_NICKNAME']);
			if(vcard_element!=null)
			{
				if(vcard_element.length!=1)	// if the NICKNAME attribute is present more than once, vCard is considered invalid
				{
					console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
					return false;
				}

				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

				tmpvCardEditorRef.find('[data-type="nickname"]').val(vcardUnescapeValue(parsed[4]));

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_NICKNAME'][0]=vCard.tplC['contentline_NICKNAME'];
				vCard.tplM['contentline_NICKNAME'][0]=vCard.tplM['contentline_NICKNAME'][0].replace('##:::##group_wd##:::##', parsed[1]);
				vCard.tplM['contentline_NICKNAME'][0]=vCard.tplM['contentline_NICKNAME'][0].replace('##:::##params_wsc##:::##', parsed[3]);

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0],'\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_NICKNAME'][0]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0],'\r\n');
					}
				}
			}
		}
//console.timeEnd('NICKNAME timer');

//console.time('X-PHONETIC-FIST-NAME timer');
		// ------------------------------------------------------------------------------------- //
		// X-PHONETIC-FIRST-NAME -> TODO: what to do if present more than once?
		if(globalDisabledContactAttributes.indexOf('X-PHONETIC-FIRST-NAME')==-1)
		{
			vcard_element=vcard.match(vCard.pre['contentline_X-PHONETIC-FIRST-NAME']);
			if(vcard_element!=null)
			{
				if(vcard_element.length!=1)	// if the X-PHONETIC-FIRST-NAME attribute is present more than once, vCard is considered invalid
				{
					console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
					return false;
				}

				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

				tmpvCardEditorRef.find('[data-type="ph_firstname"]').val(vcardUnescapeValue(parsed[4]));

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_X-PHONETIC-FIRST-NAME'][0]=vCard.tplC['contentline_X-PHONETIC-FIRST-NAME'];
				vCard.tplM['contentline_X-PHONETIC-FIRST-NAME'][0]=vCard.tplM['contentline_X-PHONETIC-FIRST-NAME'][0].replace('##:::##group_wd##:::##', parsed[1]);
				vCard.tplM['contentline_X-PHONETIC-FIRST-NAME'][0]=vCard.tplM['contentline_X-PHONETIC-FIRST-NAME'][0].replace('##:::##params_wsc##:::##', parsed[3]);

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0],'\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_X-PHONETIC-FIRST-NAME'][0]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0],'\r\n');
					}
				}
			}
		}
//console.timeEnd('X-PHONETIC-FIST-NAME timer');

//console.time('X-PHONETIC-LAST-NAME timer');
		// ------------------------------------------------------------------------------------- //
		// X-PHONETIC-LAST-NAME -> TODO: what to do if present more than once?
		if(globalDisabledContactAttributes.indexOf('X-PHONETIC-LAST-NAME')==-1)
		{
			vcard_element=vcard.match(vCard.pre['contentline_X-PHONETIC-LAST-NAME']);
			if(vcard_element!=null)
			{
				if(vcard_element.length!=1)	// if the X-PHONETIC-LAST-NAME attribute is present more than once, vCard is considered invalid
				{
					console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
					return false;
				}

				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

				tmpvCardEditorRef.find('[data-type="ph_lastname"]').val(vcardUnescapeValue(parsed[4]));

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_X-PHONETIC-LAST-NAME'][0]=vCard.tplC['contentline_X-PHONETIC-LAST-NAME'];
				vCard.tplM['contentline_X-PHONETIC-LAST-NAME'][0]=vCard.tplM['contentline_X-PHONETIC-LAST-NAME'][0].replace('##:::##group_wd##:::##', parsed[1]);
				vCard.tplM['contentline_X-PHONETIC-LAST-NAME'][0]=vCard.tplM['contentline_X-PHONETIC-LAST-NAME'][0].replace('##:::##params_wsc##:::##', parsed[3]);

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0],'\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_X-PHONETIC-LAST-NAME'][0]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0],'\r\n');
					}
				}
			}
		}
//console.timeEnd('X-PHONETIC-LAST-NAME timer');

//console.time('BDAY timer');
		// ------------------------------------------------------------------------------------- //
		// BDAY
		if(globalDisabledContactAttributes.indexOf('BDAY')==-1)
		{
			vcard_element=vcard.match(vCard.pre['contentline_BDAY']);
			if(vcard_element!=null)
			{
				if(vcard_element.length!=1)	// if the BDAY attribute is present more than once, vCard is considered invalid
				{
					console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
					return false;
				}

				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

				var valid=true;
				try {var date=$.datepicker.parseDate('yy-mm-dd', parsed[4])}
				catch (e) {valid=false}

				if(valid==true)
				{
					tmpvCardEditorRef.find('[data-type="date_bday"]').val(vcardUnescapeValue($.datepicker.formatDate(globalSettings.datepickerformat.value, date))).change();

					// values not directly supported by the editor (old values are kept intact)
					vCard.tplM['contentline_BDAY'][0]=vCard.tplC['contentline_BDAY'];
					vCard.tplM['contentline_BDAY'][0]=vCard.tplM['contentline_BDAY'][0].replace('##:::##group_wd##:::##', parsed[1]);
					vCard.tplM['contentline_BDAY'][0]=vCard.tplM['contentline_BDAY'][0].replace('##:::##params_wsc##:::##', parsed[3]);

					// remove the processed parameter
					vcard=vcard.replace(vcard_element[0],'\r\n');

					// find the corresponding group data (if exists)
					if(parsed[1]!='')
					{
						var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
						while((vcard_element_related=vcard.match(re))!=null)
						{
							// append the parameter to its parent
							vCard.tplM['contentline_BDAY'][0]+=vcard_element_related[0].substr(2);
							// remove the processed parameter
							vcard=vcard.replace(vcard_element_related[0],'\r\n');
						}
					}
				}
				else
				{
					console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
					return false;	// if the date value is invalid, vCard is considered invalid
				}
			}
		}
//console.timeEnd('BDAY timer');

//console.time('X-ABDATE timer');
		// ------------------------------------------------------------------------------------- //
		// X-ABDATE
		if(globalDisabledContactAttributes.indexOf('X-ABDATE')==-1)
		{
			var element_i=0;
			while((vcard_element=vcard.match(vCard.pre['contentline_X-ABDATE']))!=null)
			{
				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				var parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

				var valid=true;
				try {var date=$.datepicker.parseDate('yy-mm-dd', parsed[4])}
				catch (e) {valid=false}

				if(valid==true)
				{
					// parsed_value = [1..]->X-ABDATE-params
					var parsed_value=vcardSplitParam(parsed[3]);

					// click to "add" button if not enought data rows present
					var tmp_sel=tmpvCardEditorRef.find('[data-type="\\%date"]').last();
					if(tmp_sel.find('[data-type="date_value"]').val()!='')
						tmp_sel.find('[data-type="\\%add"]').find('input[type="image"]').click();

					// get the "TYPE=" values array
					var pref=0;	//by default there is no preferred date
					var type_values=Array();
					var j=0;
					for(var i=1; i<parsed_value.length; i++)
						if(parsed_value[i].toLowerCase().indexOf('type=')==0)
						{
							var type_values_tmp=parsed_value[i].substring('type='.length);	//case insensitive remove of /^type=/
							// if one value is a comma separated value of parameters
							var type_values_tmp_2=type_values_tmp.split(',');
							var type_value_tmp_2_lower='';
							for(var m=0; m<type_values_tmp_2.length; m++)
								if((type_value_tmp_2_lower=vcardUnescapeValue(type_values_tmp_2[m]).toLowerCase())!='pref')
									type_values[j++]=type_value_tmp_2_lower;
								else
									pref=1;
						}
					if(parsed[1]!='')	// APPLE SPECIFIC types: find the corresponding group.X-ABLabel: used by APPLE as "TYPE"
					{
						var vcard_element_related=null;
						var re=RegExp('\r\n'+parsed[1].replace('.','\\.X-ABLabel:(.*)')+'\r\n', 'im');
						while((vcard_element_related=vcard.match(re))!=null)
						{
							// get the X-ABLabel value
							if(type_values.indexOf(vcard_element_related[1].toLowerCase())==-1)
								type_values[j++]=vcardUnescapeValue(':'+vcard_element_related[1].toLowerCase()+':');
							// remove the processed parameter
							vcard=vcard.replace(vcard_element_related[0], '\r\n');
						}
					}

					// get the type value and label
					var type_values_us=type_values.unique().sort();
					var type_values_txt=type_values_us.join(',');	// TYPE=INTERNET;TYPE=INTERNET;TYPE=HOME; -> array('HOME','INTERNET') -> 'home,internet'
					var type_values_txt_label=type_values_us.join(' ').replace(vCard.pre['vcardToData_colon_begin_or_end'], '');	// TYPE=INTERNET;TYPE=INTERNET;TYPE=HOME; -> array('HOME','INTERNET') -> 'home internet'
					if(type_values_txt=='')	// if no person type defined, we use the 'other' type as default
						type_values_txt=type_values_txt_label='other';

					// get the default available types
					var type_list=new Array();
					tmpvCardEditorRef.find('[data-type="\\%date"]:eq('+element_i+')').find('[data-type="date_type"]').children().each(function(index, element){type_list[type_list.length]=$(element).attr('data-type');});

					// if an existing type regex matches the new type, use the old type
					// and replace the old type definition with new type definition to comforn the server vCard type format
					for(var i=0; i<type_list.length; i++)
						if(dataTypes['date_type'][type_list[i]]!=undefined && type_values_txt.match(dataTypes['date_type'][type_list[i]])!=null)
						{
							tmpvCardEditorRef.find('[data-type="\\%date"]').find('[data-type="date_type"]').find('[data-type="'+type_list[i]+'"]').attr('data-type', type_values_txt);
							break;
						}

					// date type: select or append to existing types and select
					var select_element=tmpvCardEditorRef.find('[data-type="\\%date"]:eq('+element_i+') [data-type="date_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]');
					if(select_element.length==1)
						select_element.prop('selected', true);
					else if(select_element.length==0)
					{
						// create the missing option
						var new_opt=tmpvCardEditorRef.find('[data-type="date_type"] :first-child').first().clone().attr('data-type',type_values_txt).text(type_values_txt_label);
						// append the option to all element of this type
						tmpvCardEditorRef.find('[data-type="date_type"] :last-child').prev().after(new_opt);
						// select the option on the current type
						tmpvCardEditorRef.find('[data-type="\\%date"]:eq('+element_i+') [data-type="date_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]').prop('selected', true);
					}

					tmpvCardEditorRef.find('[data-type="\\%date"]:eq('+element_i+') [data-type="date_value"]').val(vcardUnescapeValue($.datepicker.formatDate(globalSettings.datepickerformat.value, date))).change();

					// values not directly supported by the editor (old values are kept intact)
					vCard.tplM['contentline_X-ABDATE'][element_i]=vCard.tplC['contentline_X-ABDATE'];
					vCard.tplM['contentline_X-ABDATE'][element_i]=vCard.tplM['contentline_X-ABDATE'][element_i].replace('##:::##group_wd##:::##', parsed[1]);
					// if the phone person was preferred, we keep it so (we not support preferred person selection directly by editor)
					if(pref==1)
						vCard.tplM['contentline_X-ABDATE'][element_i]=vCard.tplM['contentline_X-ABDATE'][element_i].replace('##:::##params_wsc##:::##', '##:::##params_wsc##:::##;TYPE=PREF');

					// remove the processed parameter
					vcard=vcard.replace(vcard_element[0], '\r\n');

					// find the corresponding group data (if exists)
					if(parsed[1]!='')
					{
						var vcard_element_related=null;
						var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
						while((vcard_element_related=vcard.match(re))!=null)
						{
							// append the parameter to its parent
							vCard.tplM['contentline_X-ABDATE'][element_i]+=vcard_element_related[0].substr(2);
							// remove the processed parameter
							vcard=vcard.replace(vcard_element_related[0], '\r\n');
						}
					}
					element_i++;
				}
			}
		}
//console.timeEnd('X-ABDATE timer');

//console.time('TITLE timer');
		// ------------------------------------------------------------------------------------- //
		// TITLE -> TODO: what to do if present more than once?
		if(globalDisabledContactAttributes.indexOf('TITLE')==-1)
		{
			vcard_element=vcard.match(vCard.pre['contentline_TITLE']);
			if(vcard_element!=null)
			{
				if(vcard_element.length!=1)	// if the TITLE attribute is present more than once, vCard is considered invalid
				{
					console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
					return false;
				}

				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

				tmpvCardEditorRef.find('[data-type="title"]').val(vcardUnescapeValue(parsed[4]));

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_TITLE'][0]=vCard.tplC['contentline_TITLE'];
				vCard.tplM['contentline_TITLE'][0]=vCard.tplM['contentline_TITLE'][0].replace('##:::##group_wd##:::##', parsed[1]);
				vCard.tplM['contentline_TITLE'][0]=vCard.tplM['contentline_TITLE'][0].replace('##:::##params_wsc##:::##', parsed[3]);

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0],'\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_TITLE'][0]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0],'\r\n');
					}
				}
			}
		}
//console.timeEnd('TITLE timer');

//console.time('ORG timer');
		// ------------------------------------------------------------------------------------- //
		// ORG -> TODO: what to do if present more than once?
		if(globalDisabledContactAttributes.indexOf('ORG')==-1)
		{
			vcard_element=vcard.match(vCard.pre['contentline_ORG']);
			if(vcard_element!=null)
			{
				if(vcard_element.length!=1)	// if the ORG attribute is present more than once, vCard is considered invalid
				{
					console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
					return false;
				}

				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				parsed=vcard_element[0].match(vCard.pre['contentline_parse']);
				// parsed_value = [0]->Org, [1..]->Org Units
				parsed_value=vcardSplitValue(parsed[4], ';');

				if(parsed_value[0]!=undefined && parsed_value[0]!='')
					tmpvCardEditorRef.find('[data-type="org"]').val(vcardUnescapeValue(parsed_value[0]));
				if(parsed_value[1]!=undefined && parsed_value[1]!='')
					tmpvCardEditorRef.find('[data-type="department"]').val(vcardUnescapeValue(parsed_value[1]));

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_ORG'][0]=vCard.tplC['contentline_ORG'];
				vCard.tplM['contentline_ORG'][0]=vCard.tplM['contentline_ORG'][0].replace('##:::##group_wd##:::##', parsed[1]);
				vCard.tplM['contentline_ORG'][0]=vCard.tplM['contentline_ORG'][0].replace('##:::##params_wsc##:::##', parsed[3]);
				vCard.tplM['contentline_ORG'][0]=vCard.tplM['contentline_ORG'][0].replace('##:::##units_wsc##:::##', (parsed_value[2]==undefined ? '' : ';'+parsed_value.slice(2).join(';')));

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0],'\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_ORG'][0]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0],'\r\n');
					}
				}
			}
		}
//console.timeEnd('ORG timer');

//console.time('X-ABShowAs timer');
		// ------------------------------------------------------------------------------------- //
		// X-ABShowAs -> TODO: what to do if present more than once?
		var photo_show_org=false;
		if(globalDisabledContactAttributes.indexOf('X-ABShowAs')==-1)
		{
			vcard_element=vcard.match(vCard.pre['X-ABShowAs']);
			if(vcard_element!=null)
			{
				if(vcard_element.length>1)	// if the X-ABShowAs attribute is present more than once, vCard is considered invalid
				{
					console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
					return false;
				}

				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

				if(vcardUnescapeValue(parsed[4]).match(RegExp('^company$','i')))
				{
					tmpvCardEditorRef.find('[data-type="isorg"]').prop('checked', true);
					photo_show_org=true;
				}

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_X-ABShowAs'][0]=vCard.tplC['contentline_X-ABShowAs'];
				vCard.tplM['contentline_X-ABShowAs'][0]=vCard.tplM['contentline_X-ABShowAs'][0].replace('##:::##group_wd##:::##', parsed[1]);
				vCard.tplM['contentline_X-ABShowAs'][0]=vCard.tplM['contentline_X-ABShowAs'][0].replace('##:::##params_wsc##:::##', parsed[3]);
				vCard.tplM['contentline_X-ABShowAs'][0]=vCard.tplM['contentline_X-ABShowAs'][0].replace('##:::##value##:::##', parsed[4]);

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0],'\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_X-ABShowAs'][0]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0],'\r\n');
					}
				}
			}
		}
//console.timeEnd('X-ABShowAs timer');

//console.time('PHOTO timer');
		// ------------------------------------------------------------------------------------- //
		// PHOTO -> TODO: what to do if present more than once?
		if(photo_show_org)
			tmpvCardEditorRef.find('#photo').toggleClass('photo_user photo_company');

		if(globalDisabledContactAttributes.indexOf('PHOTO')==-1)
		{
			vcard_element=vcard.match(vCard.pre['contentline_PHOTO']);
			if(vcard_element!=null)	// if the PHOTO attribute is present more than once, we use the first value
			{
				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

				var img_type = '';
				var custom_params = '';
				var typeRe = RegExp('TYPE=(.*)', 'i');
				var othersRe = RegExp('(?:ENCODING|VALUE)=.*', 'i');

				parsed_value = vcardSplitParam(parsed[3]);

				for(i=1; i<parsed_value.length; i++) {
					if((type_value=parsed_value[i].match(typeRe))!=undefined) {
						img_type=type_value[1].toLowerCase();
					}
					else if(!othersRe.test(parsed_value[i])) {
						custom_params += ';'+parsed_value[i];
					}
				}

				// support also for unknown type of images (stupid clients)
				var photo = parsed[4];
				var isRemote = RegExp('^https?://', 'i').test(photo);

				var newImg = new Image();
				newImg.src = isRemote ? photo : 'data:image'+(img_type!='' ? '/'+img_type : '')+';base64,'+photo.replace(RegExp('^data:(?:image/.*?;)?(?:base64,)?','i'),'');
				newImg.onload = function(){
					loadImage(this);
				};

				if(isRemote) {
					tmpvCardEditorRef.find('#photoURL, #photoURLHidden').val(photo);
				}

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_PHOTO'][0]=vCard.tplC['contentline_PHOTO'];
				vCard.tplM['contentline_PHOTO'][0]=vCard.tplM['contentline_PHOTO'][0].replace('##:::##group_wd##:::##', parsed[1]);
				vCard.tplM['contentline_PHOTO'][0]=vCard.tplM['contentline_PHOTO'][0].replace('##:::##params_wsc##:::##', '##:::##params_wsc##:::##'+custom_params);

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0],'\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_PHOTO'][0]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0],'\r\n');
					}
				}

	//			// photo URL is used by iCloud but it requires iCloud session cookie :-(
	//			if(parsed[4].match(RegExp('^https?://','i'))!=null)
	//				tmpvCardEditorRef.find('[data-type="photo"]').attr('src',parsed[4]);
			}
			else	// use default icons (see X-ABShowAs above)
				tmpvCardEditorRef.find('#photo').addClass('photo_blank');
		}
//console.timeEnd('PHOTO timer');

//console.time('ADR timer');
		// ------------------------------------------------------------------------------------- //
		// ADR
		if(globalDisabledContactAttributes.indexOf('ADR')==-1)
		{
			var element_i=0;
			while((vcard_element=vcard.match(vCard.pre['contentline_ADR']))!=null)
			{
				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				var parsed=vcard_element[0].match(vCard.pre['contentline_parse']);
				// parsed_param = [1..]->ADR-params
				var parsed_param=vcardSplitParam(parsed[3]);
				// parsed_value = [1..]->ADR elements
				var parsed_value=vcardSplitValue(parsed[4],';');

				// click to "add" button if not enought data rows present
				var found=0;
				tmpvCardEditorRef.find('[data-type="\\%address"]').last().find('[data-type="value"]').each(
					function(index,element)
					{
						if($(element).val()!='')
						{
							found=1;
							return false;
						}
					}
				);
				if(found)
					tmpvCardEditorRef.find('[data-type="\\%address"]').last().find('[data-type="\\%add"]').find('input[type="image"]').click();

				// get the "TYPE=" values array
				var pref=0;	//by default there is no preferred address
				var type_values=Array();
				var j=0;
				for(var i=1; i<parsed_param.length; i++)
					if(parsed_param[i].toLowerCase().indexOf('type=')==0)
					{
						var type_values_tmp=parsed_param[i].substring('type='.length);	//case insensitive remove of /^type=/
						// if one value is a comma separated value of parameters
						var type_values_tmp_2=type_values_tmp.split(',');
						var type_value_tmp_2_lower='';
						for(var m=0; m<type_values_tmp_2.length; m++)
							if((type_value_tmp_2_lower=vcardUnescapeValue(type_values_tmp_2[m]).toLowerCase())!='pref')
								type_values[j++]=type_value_tmp_2_lower;
							else
								pref=1;
					}
				if(parsed[1]!='')	// APPLE SPECIFIC types: find the corresponding group.X-ABLabel: used by APPLE as "TYPE"
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\.X-ABLabel:(.*)')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// get the X-ABLabel value
						if(type_values.indexOf(vcard_element_related[1].toLowerCase())==-1)
							type_values[j++]=vcardUnescapeValue(':'+vcard_element_related[1].toLowerCase()+':');
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}
				// find the corresponding group.X-ABADR: used by APPLE as short address country
				var addr_country='';
				if(parsed[1]!='')
				{
					var re=RegExp('\r\n'+parsed[1].replace('.','\\.X-ABADR:(.*)')+'\r\n', 'm');
					if((vcard_element_related=vcard.match(re))!=null)
					{
						// get the X-ABADR value
						addr_country=vcardUnescapeValue(vcard_element_related[1]).toLowerCase();
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0],'\r\n');
					}
				}

				// get the type value and label
				var type_values_us=type_values.unique().sort();
				var type_values_txt=type_values_us.join(',');	// TYPE=HOME;TYPE=HOME;TYPE=FAX; -> array('FAX','HOME') -> 'fax,home'
				var type_values_txt_label=type_values_us.join(' ').replace(vCard.pre['vcardToData_colon_begin_or_end'], '');	// TYPE=HOME;TYPE=HOME;TYPE=FAX; -> array('FAX','HOME') -> 'fax home'
				if(type_values_txt=='')	// if no address type defined, we use the 'work' type as default
					type_values_txt=type_values_txt_label='work';

				// get the default available types
				var type_list=new Array();
				tmpvCardEditorRef.find('[data-type="\\%address"]:eq('+element_i+')').find('[data-type="address_type"]').children().each(function(index, element){type_list[type_list.length]=$(element).attr('data-type');});

				// if an existing type regex matches the new type, use the old type
				// and replace the old type definition with new type definition to comforn the server vCard type format
				for(var i=0;i<type_list.length;i++)
					if(dataTypes['address_type'][type_list[i]]!=undefined && type_values_txt.match(dataTypes['address_type'][type_list[i]])!=null)
					{
						tmpvCardEditorRef.find('[data-type="\\%address"]').find('[data-type="address_type"]').find('[data-type="'+type_list[i]+'"]').attr('data-type', type_values_txt);
						break;
					}

				// address type: select or append to existing types and select
				var select_element=tmpvCardEditorRef.find('[data-type="\\%address"]:eq('+element_i+') [data-type="address_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]');
				if(select_element.length==1)
					select_element.prop('selected', true);
				else if(select_element.length==0)
				{
					// create the missing option
					var new_opt=tmpvCardEditorRef.find('[data-type="address_type"] :first-child').first().clone().attr('data-type',type_values_txt).text(type_values_txt_label);
					// append the option to all element of this type
					tmpvCardEditorRef.find('[data-type="address_type"] :last-child').prev().after(new_opt);
					// select the option on the current type
					tmpvCardEditorRef.find('[data-type="\\%address"]:eq('+element_i+') [data-type="address_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]').prop('selected', true);
				}

				var tmp=tmpvCardEditorRef.find('[data-type="\\%address"]:eq('+element_i+')');
				var found;
				if((found=tmp.find('[data-type="country_type"]').children('[data-type="'+jqueryEscapeSelector(addr_country)+'"]')).length>0 || (found=tmp.find('[data-type="country_type"]').children('[data-full-name="'+jqueryEscapeSelector(parsed_value[6])+'"]')).length>0)
					found.prop('selected', true);
				else if(globalSettings.addresscountryequivalence.value.length>0 && parsed_value[6]!=undefined)	// unknown ADR format (country not detected)
				{
// TODO: move regex object directly into config.js
					for(var i=0; i<globalSettings.addresscountryequivalence.value.length; i++)
						if(parsed_value[6].match(RegExp(globalSettings.addresscountryequivalence.value[i].regex, 'i'))!=null)
						{
							tmp.find('[data-type="country_type"]').children('[data-type="'+jqueryEscapeSelector(globalSettings.addresscountryequivalence.value[i].country)+'"]').prop('selected', true);
							break;
						}
				}
				// Note:
				//  if no country detected, the default is used (see globalDefaultAddressCountry in config.js)

				tmp.find('[data-autoselect]').change();
				var streetVals = vcardUnescapeValue(parsed_value[2]).split('\n');

				for(var i=0; i<streetVals.length; i++) {
					var tmp = tmpvCardEditorRef.find('[data-type="\\%address"]:eq('+element_i+') [data-addr-field="street"]').last();
					tmp.val(streetVals[i]);
					if(i<streetVals.length-1) {
						tmp.trigger('keyup.street');
					}
				};

				tmpvCardEditorRef.find('[data-type="\\%address"]:eq('+element_i+') [data-addr-field="pobox"]').val(vcardUnescapeValue(parsed_value[0]));
				tmpvCardEditorRef.find('[data-type="\\%address"]:eq('+element_i+') [data-addr-field="extaddr"]').val(vcardUnescapeValue(parsed_value[1]));
				tmpvCardEditorRef.find('[data-type="\\%address"]:eq('+element_i+') [data-addr-field="locality"]').val(vcardUnescapeValue(parsed_value[3]));
				tmpvCardEditorRef.find('[data-type="\\%address"]:eq('+element_i+') [data-addr-field="region"]').val(vcardUnescapeValue(parsed_value[4]));
				tmpvCardEditorRef.find('[data-type="\\%address"]:eq('+element_i+') [data-addr-field="code"]').val(vcardUnescapeValue(parsed_value[5]));


				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_ADR'][element_i]=vCard.tplC['contentline_ADR'];
				vCard.tplM['contentline_ADR'][element_i]=vCard.tplM['contentline_ADR'][element_i].replace('##:::##group_wd##:::##', parsed[1]);
				// if the address was preferred, we keep it so (we not support preferred address selection directly by editor)
				if(pref==1)
					vCard.tplM['contentline_ADR'][element_i]=vCard.tplM['contentline_ADR'][element_i].replace('##:::##params_wsc##:::##', '##:::##params_wsc##:::##;TYPE=PREF');

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0],'\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_ADR'][element_i]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}
				element_i++;
			}
		}
//console.timeEnd('ADR timer');

//console.time('TEL timer');
		// ------------------------------------------------------------------------------------- //
		// TEL
		if(globalDisabledContactAttributes.indexOf('TEL')==-1)
		{
			var element_i=0;
			while((vcard_element=vcard.match(vCard.pre['contentline_TEL']))!=null)
			{
				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				var parsed=vcard_element[0].match(vCard.pre['contentline_parse']);
				// parsed_value = [1..]->TEL-params
				var parsed_value=vcardSplitParam(parsed[3]);

				// click to "add" button if not enought data rows present
				var tmp_sel=tmpvCardEditorRef.find('[data-type="\\%phone"]').last();
				if(tmp_sel.find('[data-type="value"]').val()!='')
					tmp_sel.find('[data-type="\\%add"]').find('input[type="image"]').click();

				// get the "TYPE=" values array
				var pref=0;	//by default there is no preferred phone number
				var type_values=Array();
				var j=0;
				for(var i=1; i<parsed_value.length; i++)
					if(parsed_value[i].toLowerCase().indexOf('type=')==0)
					{
						var type_values_tmp=parsed_value[i].substring('type='.length);	//case insensitive remove of /^type=/
						// if one value is a comma separated value of parameters
						var type_values_tmp_2=type_values_tmp.split(',');
						var type_value_tmp_2_lower='';
						for(var m=0; m<type_values_tmp_2.length; m++)
							if((type_value_tmp_2_lower=vcardUnescapeValue(type_values_tmp_2[m]).toLowerCase())!='pref')
								type_values[j++]=type_value_tmp_2_lower;
							else
								pref=1;
					}
				if(parsed[1]!='')	// APPLE SPECIFIC types: find the corresponding group.X-ABLabel: used by APPLE as "TYPE"
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\.X-ABLabel:(.*)')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// get the X-ABLabel value
						if(type_values.indexOf(vcard_element_related[1].toLowerCase())==-1)
							type_values[j++]=vcardUnescapeValue(':'+vcard_element_related[1].toLowerCase()+':');
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}

				// get the type value and label
				var type_values_us=type_values.unique().sort();
				var type_values_txt=type_values_us.join(',');	// TYPE=HOME;TYPE=HOME;TYPE=FAX; -> array('FAX','HOME') -> 'fax,home'
				var type_values_txt_label=type_values_us.join(' ').replace(vCard.pre['vcardToData_colon_begin_or_end'], '');	// TYPE=HOME;TYPE=HOME;TYPE=FAX; -> array('FAX','HOME') -> 'fax home'
				if(type_values_txt=='')	// if no phone type defined, we use the 'cell' type as default
					type_values_txt=type_values_txt_label='cell';

				// get the default available types (optimize in future)
				var type_list=new Array();
				tmpvCardEditorRef.find('[data-type="\\%phone"]:eq('+element_i+')').find('[data-type="phone_type"]').children().each(function(index, element){type_list[type_list.length]=$(element).attr('data-type');});

				// if an existing type regex matches the new type, use the old type
				// and replace the old type definition with new type definition to comforn the current vCard type format
				for(var i=0; i<type_list.length; i++)
					if(dataTypes['phone_type'][type_list[i]]!=undefined && type_values_txt.match(dataTypes['phone_type'][type_list[i]])!=null)
					{
						tmpvCardEditorRef.find('[data-type="\\%phone"]').find('[data-type="phone_type"]').find('[data-type="'+type_list[i]+'"]').attr('data-type', type_values_txt);
						break;
					}

				// phone type: select or append to existing types and select
				var select_element=tmpvCardEditorRef.find('[data-type="\\%phone"]:eq('+element_i+') [data-type="phone_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]');
				if(select_element.length==1)
					select_element.prop('selected', true);
				else if(select_element.length==0)
				{
					// create the missing option
					var new_opt=tmpvCardEditorRef.find('[data-type="phone_type"] :first-child').first().clone().attr('data-type', type_values_txt).text(type_values_txt_label);
					// append the option to all element of this type
					tmpvCardEditorRef.find('[data-type="phone_type"] :last-child').prev().after(new_opt);
					// select the option on the current type
					tmpvCardEditorRef.find('[data-type="\\%phone"]:eq('+element_i+') [data-type="phone_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]').prop('selected', true);
				}

				tmpvCardEditorRef.find('[data-type="\\%phone"]:eq('+element_i+') [data-type="value"]').val(vcardUnescapeValue(parsed[4]));

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_TEL'][element_i]=vCard.tplC['contentline_TEL'];
				vCard.tplM['contentline_TEL'][element_i]=vCard.tplM['contentline_TEL'][element_i].replace('##:::##group_wd##:::##', parsed[1]);
				// if the phone number was preferred, we keep it so (we not support preferred number selection directly by editor)
				if(pref==1)
					vCard.tplM['contentline_TEL'][element_i]=vCard.tplM['contentline_TEL'][element_i].replace('##:::##params_wsc##:::##', '##:::##params_wsc##:::##;TYPE=PREF');

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0], '\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_TEL'][element_i]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}
				element_i++;
			}
		}
//console.timeEnd('TEL timer');

//console.time('EMAIL timer');
		// ------------------------------------------------------------------------------------- //
		// EMAIL
		if(globalDisabledContactAttributes.indexOf('EMAIL')==-1)
		{
			var element_i=0;
			while((vcard_element=vcard.match(vCard.pre['contentline_EMAIL']))!=null)
			{
				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				var parsed=vcard_element[0].match(vCard.pre['contentline_parse']);
				// parsed_value = [1..]->EMAIL-params
				var parsed_value=vcardSplitParam(parsed[3]);

				// click to "add" button if not enought data rows present
				var tmp_sel=tmpvCardEditorRef.find('[data-type="\\%email"]').last();
				if(tmp_sel.find('[data-type="value"]').val()!='')
					tmp_sel.find('[data-type="\\%add"]').find('input[type="image"]').click();

				// get the "TYPE=" values array
				var pref=0;	//by default there is no preferred email address
				var type_values=Array();
				var j=0;
				for(var i=1; i<parsed_value.length; i++)
					if(parsed_value[i].toLowerCase().indexOf('type=')==0)
					{
						var type_values_tmp=parsed_value[i].substring('type='.length);	//case insensitive remove of /^type=/
						// if one value is a comma separated value of parameters
						var type_values_tmp_2=type_values_tmp.split(',');
						var type_value_tmp_2_lower='';
						for(var m=0; m<type_values_tmp_2.length; m++)
							if((type_value_tmp_2_lower=vcardUnescapeValue(type_values_tmp_2[m]).toLowerCase())!='pref')
								type_values[j++]=type_value_tmp_2_lower;
							else
								pref=1;
					}
				if(parsed[1]!='')	// APPLE SPECIFIC types: find the corresponding group.X-ABLabel: used by APPLE as "TYPE"
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\.X-ABLabel:(.*)')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// get the X-ABLabel value
						if(type_values.indexOf(vcard_element_related[1].toLowerCase())==-1)
							type_values[j++]=vcardUnescapeValue(':'+vcard_element_related[1].toLowerCase()+':');
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}

				// get the type value and label
				var type_values_us=type_values.unique().sort();
				var type_values_txt=type_values_us.join(',');	// TYPE=INTERNET;TYPE=INTERNET;TYPE=HOME; -> array('HOME','INTERNET') -> 'home,internet'
				var type_values_txt_label=type_values_us.join(' ').replace(vCard.pre['vcardToData_colon_begin_or_end'], '');	// TYPE=INTERNET;TYPE=INTERNET;TYPE=HOME; -> array('HOME','INTERNET') -> 'home internet'
				if(type_values_txt=='')	// if no email type defined, we use the 'home' type as default
					type_values_txt=type_values_txt_label='home,internet';

				// get the default available types
				var type_list=new Array();
				tmpvCardEditorRef.find('[data-type="\\%email"]:eq('+element_i+')').find('[data-type="email_type"]').children().each(function(index, element){type_list[type_list.length]=$(element).attr('data-type');});

				// if an existing type regex matches the new type, use the old type
				// and replace the old type definition with new type definition to comforn the server vCard type format
				for(var i=0; i<type_list.length; i++)
					if(dataTypes['email_type'][type_list[i]]!=undefined && type_values_txt.match(dataTypes['email_type'][type_list[i]])!=null)
					{
						tmpvCardEditorRef.find('[data-type="\\%email"]').find('[data-type="email_type"]').find('[data-type="'+type_list[i]+'"]').attr('data-type', type_values_txt);
						break;
					}

				// email type: select or append to existing types and select
				var select_element=tmpvCardEditorRef.find('[data-type="\\%email"]:eq('+element_i+') [data-type="email_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]');
				if(select_element.length==1)
					select_element.prop('selected',true);
				else if(select_element.length==0)
				{
					// create the missing option
					new_opt=tmpvCardEditorRef.find('[data-type="email_type"] :first-child').first().clone().attr('data-type',type_values_txt).text(type_values_txt_label);
					// append the option to all element of this type
					tmpvCardEditorRef.find('[data-type="email_type"] :last-child').prev().after(new_opt);
					// select the option on the current type
					tmpvCardEditorRef.find('[data-type="\\%email"]:eq('+element_i+') [data-type="email_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]').prop('selected',true);
				}
				tmpvCardEditorRef.find('[data-type="\\%email"]:eq('+element_i+') [data-type="value"]').val(vcardUnescapeValue(parsed[4]));

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_EMAIL'][element_i]=vCard.tplC['contentline_EMAIL'];
				vCard.tplM['contentline_EMAIL'][element_i]=vCard.tplM['contentline_EMAIL'][element_i].replace('##:::##group_wd##:::##', parsed[1]);
				// if the phone number was preferred, we keep it so (we not support preferred number selection directly by editor)
				if(pref==1)
					vCard.tplM['contentline_EMAIL'][element_i]=vCard.tplM['contentline_EMAIL'][element_i].replace('##:::##params_wsc##:::##', '##:::##params_wsc##:::##;TYPE=PREF');

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0], '\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_EMAIL'][element_i]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}
				element_i++;
			}
		}
//console.timeEnd('EMAIL timer');

//console.time('X-SOCIALPROFILE timer');
		// ------------------------------------------------------------------------------------- //
		// X-SOCIALPROFILE
		if(globalDisabledContactAttributes.indexOf('X-SOCIALPROFILE')==-1)
		{
			var element_i=0;
			while((vcard_element=vcard.match(vCard.pre['contentline_X-SOCIALPROFILE']))!=null)
			{
				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				var parsed=vcard_element[0].match(vCard.pre['contentline_parse']);
				// parsed_value = [1..]->X-SOCIALPROFILE-params
				var parsed_value=vcardSplitParam(parsed[3]);

				// click to "add" button if not enought data rows present
				var tmp_sel=tmpvCardEditorRef.find('[data-type="\\%profile"]').last();
				if(tmp_sel.find('[data-type="value"]').val()!='')
					tmp_sel.find('[data-type="\\%add"]').find('input[type="image"]').click();

				// get the "TYPE=" values array
				var pref=0;	//by default there is no preferred X-SOCIALPROFILE
				var type_values=Array();
				var j=0;
				var social_user='';
				for(i=1;i<parsed_value.length;i++)
					if(parsed_value[i].toLowerCase().indexOf('x-user=')==0)
						social_user=parsed_value[i].substring('x-user='.length);	//case insensitive remove of /^x-user=/
					else if(parsed_value[i].toLowerCase().indexOf('type=')==0)
					{
						var type_values_tmp=parsed_value[i].substring('type='.length);	//case insensitive remove of /^type=/
						// if one value is a comma separated value of parameters
						var type_values_tmp_2=type_values_tmp.split(',');
						var type_value_tmp_2_lower='';
						for(var m=0; m<type_values_tmp_2.length; m++)
							if((type_value_tmp_2_lower=vcardUnescapeValue(type_values_tmp_2[m]).toLowerCase())!='pref')
								type_values[j++]=type_value_tmp_2_lower;
							else
								pref=1;
					}
				// if there is no x-user parameter extract the username from the URL (last part of the URL before '/')
				if(social_user=='')
					social_user=parsed[4].split('/').slice(-2)[0];

				// get the type value and label
				var type_values_us=type_values.unique().sort();
				var type_values_txt=type_values_us.join(',');	// TYPE=B;TYPE=A;TYPE=C; -> array('B','A','C') -> 'a,b,c'
				var type_values_txt_label=type_values_us.join(' ').replace(vCard.pre['vcardToData_colon_begin_or_end'], '');	// TYPE=B;TYPE=A;TYPE=C; -> array('B','A','C') -> 'a b c'
				if(type_values_txt=='')	// if no X-SOCIALPROFILE type defined, we use the 'twitter' type as default
					type_values_txt=type_values_txt_label='twitter';

				// get the default available types
				var type_list=new Array();
				tmpvCardEditorRef.find('[data-type="\\%profile"]:eq('+element_i+')').find('[data-type="profile_type"]').children().each(function(index, element){type_list[type_list.length]=$(element).attr('data-type');});

				// if an existing type regex matches the new type, use the old type
				// and replace the old type definition with new type definition to comforn the server vCard type format
				for(var i=0; i<type_list.length; i++)
					if(dataTypes['profile_type'][type_list[i]]!=undefined && type_values_txt.match(dataTypes['profile_type'][type_list[i]])!=null)
					{
						tmpvCardEditorRef.find('[data-type="\\%profile"]').find('[data-type="profile_type"]').find('[data-type="'+type_list[i]+'"]').attr('data-type', type_values_txt);
						break;
					}

				// X-SOCIALPROFILE type: select or append to existing types and select
				var select_element=tmpvCardEditorRef.find('[data-type="\\%profile"]:eq('+element_i+') [data-type="profile_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]');
				if(select_element.length==1)
					select_element.prop('selected',true);
				else if(select_element.length==0)
				{
					// create the missing option
					new_opt=tmpvCardEditorRef.find('[data-type="profile_type"] :first-child').first().clone().attr('data-type',type_values_txt).text(type_values_txt_label);
					// append the option to all element of this type
					tmpvCardEditorRef.find('[data-type="profile_type"] :last-child').prev().after(new_opt);
					// select the option on the current type
					tmpvCardEditorRef.find('[data-type="\\%profile"]:eq('+element_i+') [data-type="profile_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]').prop('selected', true);
				}
				tmpvCardEditorRef.find('[data-type="\\%profile"]:eq('+element_i+') [data-type="value"]').val(vcardUnescapeValue(type_values_txt=='twitter' ? '@'+social_user : social_user));

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_X-SOCIALPROFILE'][element_i]=vCard.tplC['contentline_X-SOCIALPROFILE'];
				vCard.tplM['contentline_X-SOCIALPROFILE'][element_i]=vCard.tplM['contentline_X-SOCIALPROFILE'][element_i].replace('##:::##group_wd##:::##', parsed[1]);
				// if the X-SOCIALPROFILE was preferred, we keep it so (we not support preferred X-SOCIALPROFILE selection directly by editor)
				if(pref==1)
					vCard.tplM['contentline_X-SOCIALPROFILE'][element_i]=vCard.tplM['contentline_X-SOCIALPROFILE'][element_i].replace('##:::##params_wsc##:::##', '##:::##params_wsc##:::##;TYPE=PREF');

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0], '\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_X-SOCIALPROFILE'][element_i]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}
				element_i++;
			}
		}
//console.timeEnd('X-SOCIALPROFILE timer');

//console.time('URL timer');
		// ------------------------------------------------------------------------------------- //
		// URL
		if(globalDisabledContactAttributes.indexOf('URL')==-1)
		{
			var element_i=0;
			while((vcard_element=vcard.match(vCard.pre['contentline_URL']))!=null)
			{
				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				var parsed=vcard_element[0].match(vCard.pre['contentline_parse']);
				// parsed_value = [1..]->URL-params
				var parsed_value=vcardSplitParam(parsed[3]);

				// click to "add" button if not enought data rows present
				var tmp_sel=tmpvCardEditorRef.find('[data-type="\\%url"]').last();
				if(tmp_sel.find('[data-type="value"]').val()!='')
					tmp_sel.find('[data-type="\\%add"]').find('input[type="image"]').click();

				// get the "TYPE=" values array
				var pref=0;	//by default there is no preferred url address
				var type_values=Array();
				var j=0;
				for(var i=1; i<parsed_value.length; i++)
					if(parsed_value[i].toLowerCase().indexOf('type=')==0)
					{
						var type_values_tmp=parsed_value[i].substring('type='.length);	//case insensitive remove of /^type=/
						// if one value is a comma separated value of parameters
						var type_values_tmp_2=type_values_tmp.split(',');
						var type_value_tmp_2_lower='';
						for(var m=0; m<type_values_tmp_2.length; m++)
							if((type_value_tmp_2_lower=vcardUnescapeValue(type_values_tmp_2[m]).toLowerCase())!='pref')
								type_values[j++]=type_value_tmp_2_lower;
							else
								pref=1;
					}
				if(parsed[1]!='')	// APPLE SPECIFIC types: find the corresponding group.X-ABLabel: used by APPLE as "TYPE"
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\.X-ABLabel:(.*)')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// get the X-ABLabel value
						if(type_values.indexOf(vcard_element_related[1].toLowerCase())==-1)
							type_values[j++]=vcardUnescapeValue(':'+vcard_element_related[1].toLowerCase()+':');
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}

				// get the type value and label
				var type_values_us=type_values.unique().sort();
				var type_values_txt=type_values_us.join(',');	// TYPE=WORK;TYPE=WORK;TYPE=HOME; -> array('HOME','WORK') -> 'home,work'
				var type_values_txt_label=type_values_us.join(' ').replace(vCard.pre['vcardToData_colon_begin_or_end'], '');	// TYPE=WORK;TYPE=WORK;TYPE=HOME; -> array('HOME','WORK') -> 'home work'
				if(type_values_txt=='')	// if no url type defined, we use the 'homepage' type as default
					type_values_txt=type_values_txt_label='homepage';

				// get the default available types (optimize in future)
				var type_list=new Array();
				tmpvCardEditorRef.find('[data-type="\\%url"]:eq('+element_i+')').find('[data-type="url_type"]').children().each(function(index, element){type_list[type_list.length]=$(element).attr('data-type');});

				// if an existing type regex matches the new type, use the old type
				// and replace the old type definition with new type definition to comforn the server vCard type format
				for(var i=0; i<type_list.length; i++)
					if(dataTypes['url_type'][type_list[i]]!=undefined && type_values_txt.match(dataTypes['url_type'][type_list[i]])!=null)
					{
						tmpvCardEditorRef.find('[data-type="\\%url"]').find('[data-type="url_type"]').find('[data-type="'+type_list[i]+'"]').attr('data-type', type_values_txt);
						break;
					}

				// url type: select or append to existing types and select
				var select_element=tmpvCardEditorRef.find('[data-type="\\%url"]:eq('+element_i+') [data-type="url_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]');
				if(select_element.length==1)
					select_element.prop('selected', true);
				else if(select_element.length==0)
				{
					// create the missing option
					var new_opt=tmpvCardEditorRef.find('[data-type="url_type"] :first-child').first().clone().attr('data-type',type_values_txt).text(type_values_txt_label);
					// append the option to all element of this type
					tmpvCardEditorRef.find('[data-type="url_type"] :last-child').prev().after(new_opt);
					// select the option on the current type
					tmpvCardEditorRef.find('[data-type="\\%url"]:eq('+element_i+') [data-type="url_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]').prop('selected', true);
				}

				tmpvCardEditorRef.find('[data-type="\\%url"]:eq('+element_i+') [data-type="value"]').val(vcardUnescapeValue(parsed[4]));

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_URL'][element_i]=vCard.tplC['contentline_URL'];
				vCard.tplM['contentline_URL'][element_i]=vCard.tplM['contentline_URL'][element_i].replace('##:::##group_wd##:::##', parsed[1]);
				// if the URL was preferred, we keep it so (we not support preferred number selection directly by editor)
				if(pref==1)
					vCard.tplM['contentline_URL'][element_i]=vCard.tplM['contentline_URL'][element_i].replace('##:::##params_wsc##:::##', '##:::##params_wsc##:::##;TYPE=PREF');

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0], '\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_URL'][element_i]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}
				element_i++;
			}
		}
//console.timeEnd('URL timer');
//
//console.time('X-ABRELATEDNAMES timer');
		// ------------------------------------------------------------------------------------- //
		// X-ABRELATEDNAMES
		if(globalDisabledContactAttributes.indexOf('X-ABRELATEDNAMES')==-1)
		{
			var element_i=0;
			while((vcard_element=vcard.match(vCard.pre['contentline_X-ABRELATEDNAMES']))!=null)
			{
				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				var parsed=vcard_element[0].match(vCard.pre['contentline_parse']);
				// parsed_value = [1..]->X-ABRELATEDNAMES-params
				var parsed_value=vcardSplitParam(parsed[3]);

				// click to "add" button if not enought data rows present
				var tmp_sel=tmpvCardEditorRef.find('[data-type="\\%person"]').last();
				if(tmp_sel.find('[data-type="value"]').val()!='')
					tmp_sel.find('[data-type="\\%add"]').find('input[type="image"]').click();

				// get the "TYPE=" values array
				var pref=0;	//by default there is no preferred person
				var type_values=Array();
				var j=0;
				for(var i=1; i<parsed_value.length; i++)
					if(parsed_value[i].toLowerCase().indexOf('type=')==0)
					{
						var type_values_tmp=parsed_value[i].substring('type='.length);	//case insensitive remove of /^type=/
						// if one value is a comma separated value of parameters
						var type_values_tmp_2=type_values_tmp.split(',');
						var type_value_tmp_2_lower='';
						for(var m=0; m<type_values_tmp_2.length; m++)
							if((type_value_tmp_2_lower=vcardUnescapeValue(type_values_tmp_2[m]).toLowerCase())!='pref')
								type_values[j++]=type_value_tmp_2_lower;
							else
								pref=1;
					}
				if(parsed[1]!='')	// APPLE SPECIFIC types: find the corresponding group.X-ABLabel: used by APPLE as "TYPE"
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\.X-ABLabel:(.*)')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// get the X-ABLabel value
						if(type_values.indexOf(vcard_element_related[1].toLowerCase())==-1)
							type_values[j++]=vcardUnescapeValue(':'+vcard_element_related[1].toLowerCase()+':');
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}

				// get the type value and label
				var type_values_us=type_values.unique().sort();
				var type_values_txt=type_values_us.join(',');	// TYPE=INTERNET;TYPE=INTERNET;TYPE=HOME; -> array('HOME','INTERNET') -> 'home,internet'
				var type_values_txt_label=type_values_us.join(' ').replace(vCard.pre['vcardToData_colon_begin_or_end'], '');	// TYPE=INTERNET;TYPE=INTERNET;TYPE=HOME; -> array('HOME','INTERNET') -> 'home internet'
				if(type_values_txt=='')	// if no person type defined, we use the 'other' type as default
					type_values_txt=type_values_txt_label='other';

				// get the default available types
				var type_list=new Array();
				tmpvCardEditorRef.find('[data-type="\\%person"]:eq('+element_i+')').find('[data-type="person_type"]').children().each(function(index, element){type_list[type_list.length]=$(element).attr('data-type');});

				// if an existing type regex matches the new type, use the old type
				// and replace the old type definition with new type definition to comforn the server vCard type format
				for(var i=0; i<type_list.length; i++)
					if(dataTypes['person_type'][type_list[i]]!=undefined && type_values_txt.match(dataTypes['person_type'][type_list[i]])!=null)
					{
						tmpvCardEditorRef.find('[data-type="\\%person"]').find('[data-type="person_type"]').find('[data-type="'+type_list[i]+'"]').attr('data-type', type_values_txt);
						break;
					}

				// person type: select or append to existing types and select
				var select_element=tmpvCardEditorRef.find('[data-type="\\%person"]:eq('+element_i+') [data-type="person_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]');
				if(select_element.length==1)
					select_element.prop('selected', true);
				else if(select_element.length==0)
				{
					// create the missing option
					var new_opt=tmpvCardEditorRef.find('[data-type="person_type"] :first-child').first().clone().attr('data-type',type_values_txt).text(type_values_txt_label);
					// append the option to all element of this type
					tmpvCardEditorRef.find('[data-type="person_type"] :last-child').prev().after(new_opt);
					// select the option on the current type
					tmpvCardEditorRef.find('[data-type="\\%person"]:eq('+element_i+') [data-type="person_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]').prop('selected', true);
				}

				tmpvCardEditorRef.find('[data-type="\\%person"]:eq('+element_i+') [data-type="value"]').val(vcardUnescapeValue(parsed[4]));

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_X-ABRELATEDNAMES'][element_i]=vCard.tplC['contentline_X-ABRELATEDNAMES'];
				vCard.tplM['contentline_X-ABRELATEDNAMES'][element_i]=vCard.tplM['contentline_X-ABRELATEDNAMES'][element_i].replace('##:::##group_wd##:::##', parsed[1]);
				// if the phone person was preferred, we keep it so (we not support preferred person selection directly by editor)
				if(pref==1)
					vCard.tplM['contentline_X-ABRELATEDNAMES'][element_i]=vCard.tplM['contentline_X-ABRELATEDNAMES'][element_i].replace('##:::##params_wsc##:::##', '##:::##params_wsc##:::##;TYPE=PREF');

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0], '\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_X-ABRELATEDNAMES'][element_i]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}
				element_i++;
			}
		}
//console.timeEnd('X-ABRELATEDNAMES timer');

//console.time('IMPP timer');
		// ------------------------------------------------------------------------------------- //
		// IMPP
		if(globalDisabledContactAttributes.indexOf('IMPP')==-1)
		{
			var element_i=0;
			while((vcard_element=vcard.match(vCard.pre['contentline_IMPP']))!=null)
			{
				// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
				var parsed=vcard_element[0].match(vCard.pre['contentline_parse']);
				// parsed_value = [1..]->IMPP-params
				var parsed_value=vcardSplitParam(parsed[3]);

				// click to "add" button if not enought data rows present
				var tmp_sel=tmpvCardEditorRef.find('[data-type="\\%im"]').last();
				if(tmp_sel.find('[data-type="value"]').val()!='')
					tmp_sel.find('[data-type="\\%add"]').find('input[type="image"]').click();

				// get the "TYPE=" & "X-SERVICE-TYPE" values array
				var pref=0;	//by default there is no preferred IM
				var type_values=Array();
				var j=0;
				var service_type_value='';
				for(var i=1; i<parsed_value.length; i++)
					if(parsed_value[i].toLowerCase().indexOf('type=')==0)
					{
						var type_values_tmp=parsed_value[i].substring('type='.length);	//case insensitive remove of /^type=/
						// if one value is a comma separated value of parameters
						var type_values_tmp_2=type_values_tmp.split(',');
						var type_value_tmp_2_lower='';
						for(var m=0; m<type_values_tmp_2.length; m++)
							if((type_value_tmp_2_lower=vcardUnescapeValue(type_values_tmp_2[m]).toLowerCase())!='pref')
								type_values[j++]=type_value_tmp_2_lower;
							else
								pref=1;
					}
					else if(parsed_value[i].toLowerCase().indexOf('x-service-type=')==0)
						service_type_value=vcardUnescapeValue(parsed_value[i].substring('x-service-type='.length)).toLowerCase();	//case insensitive remove of /^x-service-type=/
				if(parsed[1]!='')	// APPLE SPECIFIC types: find the corresponding group.X-ABLabel: used by APPLE as "TYPE"
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\.X-ABLabel:(.*)')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// get the X-ABLabel value
						if(type_values.indexOf(vcard_element_related[1].toLowerCase())==-1)
							type_values[j++]=vcardUnescapeValue(':'+vcard_element_related[1].toLowerCase()+':');
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}

				// get the type value and label
				var type_values_us=type_values.unique().sort();
				type_values_txt=type_values_us.join(',');	// TYPE=INTERNET;TYPE=INTERNET;TYPE=HOME; -> array('HOME','INTERNET') -> 'home,internet'
				type_values_txt_label=type_values_us.join(' ').replace(vCard.pre['vcardToData_colon_begin_or_end'], '');	// TYPE=INTERNET;TYPE=INTERNET;TYPE=HOME; -> array('HOME','INTERNET') -> 'home internet'
				if(type_values_txt=='')	// if no IMPP type defined, we use the 'other' type as default
					type_values_txt=type_values_txt_label='other';

				// get the default available types
				var type_list=new Array();
				tmpvCardEditorRef.find('[data-type="\\%im"]:eq('+element_i+')').find('[data-type="im_type"]').children().each(function(index, element){type_list[type_list.length]=$(element).attr('data-type');});

				// if an existing type regex matches the new type, use the old type
				// and replace the old type definition with new type definition to comforn the server vCard type format
				for(var i=0; i<type_list.length; i++)
					if(dataTypes['im_type'][type_list[i]]!=undefined && type_values_txt.match(dataTypes['im_type'][type_list[i]])!=null)
					{
						tmpvCardEditorRef.find('[data-type="\\%im"]').find('[data-type="im_type"]').find('[data-type="'+type_list[i]+'"]').attr('data-type', type_values_txt);
						break;
					}

				// IM type: select or append to existing types and select
				var select_element=tmpvCardEditorRef.find('[data-type="\\%im"]:eq('+element_i+') [data-type="im_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]');
				if(select_element.length==1)
					select_element.prop('selected',true);
				else if(select_element.length==0)
				{
					// create the missing option
					var new_opt=tmpvCardEditorRef.find('[data-type="im_type"] :first-child').first().clone().attr('data-type',type_values_txt).text(type_values_txt_label);
					// append the option to all element of this type
					tmpvCardEditorRef.find('[data-type="im_type"] :last-child').prev().after(new_opt);
					// select the option on the current type
					tmpvCardEditorRef.find('[data-type="\\%im"]:eq('+element_i+') [data-type="im_type"]').find('[data-type="'+jqueryEscapeSelector(type_values_txt)+'"]').prop('selected', true);
				}
				// IM service type: select or append to existing types and select
				select_element=tmpvCardEditorRef.find('[data-type="\\%im"]:eq('+element_i+') [data-type="im_service_type"]').find('[data-type="'+jqueryEscapeSelector(service_type_value)+'"]');
				if(select_element.length==1)
					select_element.prop('selected',true);
				else if(select_element.length==0)
				{
					// create the missing option
					new_opt=tmpvCardEditorRef.find('[data-type="im_service_type"] :first-child').first().clone().attr('data-type',service_type_value).text(service_type_value);
					// append the option to all element of this type
					tmpvCardEditorRef.find('[data-type="im_service_type"] :last-child').prev().after(new_opt);
					// select the option on the current type
					tmpvCardEditorRef.find('[data-type="\\%im"]:eq('+element_i+') [data-type="im_service_type"]').find('[data-type="'+jqueryEscapeSelector(service_type_value)+'"]').prop('selected', true);
				}

				tmpvCardEditorRef.find('[data-type="\\%im"]:eq('+element_i+') [data-type="value"]').val(vcardUnescapeValue(parsed[4].replace(vCard.pre['vcardToData_before_val'], '')));

				// values not directly supported by the editor (old values are kept intact)
				vCard.tplM['contentline_IMPP'][element_i]=vCard.tplC['contentline_IMPP'];
				vCard.tplM['contentline_IMPP'][element_i]=vCard.tplM['contentline_IMPP'][element_i].replace('##:::##group_wd##:::##', parsed[1]);
				// if the IMPP accound was preferred, we keep it so (we not support preferred person selection directly by editor)
				if(pref==1)
					vCard.tplM['contentline_IMPP'][element_i]=vCard.tplM['contentline_IMPP'][element_i].replace('##:::##params_wsc##:::##', '##:::##params_wsc##:::##;TYPE=PREF');

				// remove the processed parameter
				vcard=vcard.replace(vcard_element[0], '\r\n');

				// find the corresponding group data (if exists)
				if(parsed[1]!='')
				{
					var vcard_element_related=null;
					var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'im');
					while((vcard_element_related=vcard.match(re))!=null)
					{
						// append the parameter to its parent
						vCard.tplM['contentline_IMPP'][element_i]+=vcard_element_related[0].substr(2);
						// remove the processed parameter
						vcard=vcard.replace(vcard_element_related[0], '\r\n');
					}
				}
				element_i++;
			}
		}
//console.timeEnd('IMPP timer');

		// extension hook
		if(typeof(globalContactsExtVcardToData)=='function')
			vcard=globalContactsExtVcardToData(tmpvCardEditorRef, inputContact, vcard);

		// ------------------------------------------------------------------------------------- //
		// Store the vCard URL to XML
		tmpvCardEditorRef.find('#vCardEditor').attr('data-account-uid', inputContact.accountUID);
		tmpvCardEditorRef.find('#vCardEditor').attr('data-url', inputContact.uid);
		tmpvCardEditorRef.find('#vCardEditor').attr('data-etag', inputContact.etag);

		// UID is stored also in the Cancel button (for Add -> Cancel support /loading the previous active contact/)
		if(inputContact.uid!=undefined)	// occurs if loadContactByVcard is used (it also appends the UID of previous contact into 'data-id')
			tmpvCardEditorRef.find('#vCardEditor').find('[data-type="cancel"]').attr('data-id', inputContact.uid);

		processEditorElements(tmpvCardEditorRef, inputEditorMode, inputIsReadonly, inputContact.isCompany);

		var tmp_optionslist=[];
		// create the list of available collections to the interface
		for(var i=0; i<globalResourceCardDAVList.collections.length; i++)
			if(globalResourceCardDAVList.collections[i].headerOnly!==true && globalResourceCardDAVList.collections[i].makeLoaded===true)
				tmp_optionslist[tmp_optionslist.length]=$('<option data-type=""></option>').attr({'data-type': globalResourceCardDAVList.collections[i].uid, 'data-color': globalResourceCardDAVList.collections[i].color}).text(globalResourceCardDAVList.collections[i].displayvalue);
		// add the list of available collections to the interface
		tmpvCardEditorRef.find('[data-attr-name="_DEST_"]').append(tmp_optionslist);
		// bind the change event (color change in the editor)
		tmpvCardEditorRef.find('[data-attr-name="_DEST_"]').change(function(){
			var selColl=globalResourceCardDAVList.getCollectionByUID($(this).find('option:selected').attr('data-type'));
			globalRefAddContact.attr('data-url', selColl.uid.replace(RegExp('[^/]+$'),''));
			globalRefAddContact.attr('data-filter-url',selColl.uid);	// Set the current addressbook filter uid
			globalRefAddContact.attr('data-account-uid',selColl.accountUID);
			$('#ABContactColor').css('background-color', $(this).find('option:selected').attr('data-color'));
		});

		var collUID='';
		if(typeof inputContact.uid!='undefined')
			 collUID= inputContact.uid.replace(RegExp('[^/]*$'),'');
		else
			collUID = globalRefAddContact.attr('data-url');
		var select_elem=tmpvCardEditorRef.find('[data-attr-name="_DEST_"]').find('[data-type="'+jqueryEscapeSelector(collUID)+'"]');
		if(select_elem.length==1)
			select_elem.prop('selected', true);

		if(typeof globalContactsExtVcardToData!='undefined' && !inputIsCompany)
			tmpvCardEditorRef.find('[data-type="DEST"]').addClass('element_no_display');

		// Unprocessed unrelated vCard elements
		vCard.tplM['unprocessed_unrelated']=vcard;

		if(typeof globalDebug!='undefined' && globalDebug instanceof Array && globalDebug.indexOf('vcard')!=-1)
		{
			console.timeEnd('vcardToData timer');

			if(vcard!='\r\n')
				console.log('Warning: [vCard unprocessed unrelated]: '+vcard);
		}

		//clean error message
		$('#ABMessage').height('0');

		$('#ABContact').empty().append(tmpvCardEditorRef);

		var foundGroup=0;
		for(var adr in globalAddressbookList.vcard_groups)
		{
			if(globalAddressbookList.vcard_groups[adr].length>0)
			{
				foundGroup=1;
				break;
			}
		}

		if(foundGroup)
		{
			if(typeof inputContact.uid!='undefined')
				extendDestSelect();
			else
			{
				var selGroup = $('#ResourceCardDAVList').find('.contact_group').find(':input.resourceCardDAV_selected').attr('data-id');
				extendDestSelect(selGroup);
				if(typeof selGroup!= 'undefined')
					select_elem.text(localization[globalInterfaceLanguage].txtVcardGroupsTextSingle.replace('%coll%',globalResourceCardDAVList.getCollectionByUID(collUID).displayvalue));
			}
		}
		if(typeof inputContact.uid !='undefined')
			checkForVcardGroups(inputContact.uid);
		if(typeof(globalContactsSelectProcess)=='function')
			globalContactsSelectProcess(tmpvCardEditorRef, inputContact);

		return true;
	}
	else
	{
		console.log("Error: '"+inputContact.uid+"': unable to parse vCard");
		return false;
	}
}

function basicRFCFixesAndCleanup(vcardString)
{
	// If vCard contains only '\n' instead of '\r\n' we fix it
	if(vcardString.match(vCard.pre['basicRFCFixesAndCleanup_r-m'])==null)
		vcardString=vcardString.replace(vCard.pre['basicRFCFixesAndCleanup_n-gm'], '\r\n');

	// remove multiple empty lines
	vcardString=vcardString.replace(vCard.pre['basicRFCFixesAndCleanup_rnp-gm'], '\r\n');

	// append '\r\n' to the end of the vCard if missing
	if(vcardString[vcardString.length-1]!='\n')
		vcardString+='\r\n';

	// remove line folding
	vcardString=vcardString.replace(vCard.pre['basicRFCFixesAndCleanup_rnwsp-gm'], '');

	// RFC-obsolete PHOTO fix
	vcardString=vcardString.replace(vCard.pre['basicRFCFixesAndCleanup_photo-gim'], '\r\nPHOTO:');

	// ------------------------------------------------------------------------------------- //
	// begin CATEGORIES merge to one CATEGORIES attribute (sorry for related attributes)
	// note: we cannot do this in additionalRFCFixes or normalizeVcard
	var categoriesArr=[];
	var vcard_element=null;
	var vcard_element_related=null;
	while((vcard_element=vcardString.match(vCard.pre['contentline_CATEGORIES']))!=null)
	{
		// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
		var parsed=vcard_element[0].match(vCard.pre['contentline_parse']);

		categoriesArr[categoriesArr.length]=parsed[4];

		// remove the processed parameter
		vcardString=vcardString.replace(vcard_element[0],'\r\n');

		// find the corresponding group data (if exists)
		if(parsed[1]!='')
		{
			var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'm');
			while((vcard_element_related=vcardString.match(re))!=null)
				// remove the processed parameter
				vcardString=vcardString.replace(vcard_element_related[0],'\r\n');
		}
	}
	var categoriesTxt=categoriesArr.join(',');

	var tmp=vcardString.split('\r\n');
	tmp.splice(tmp.length-2,0,'CATEGORIES:'+categoriesTxt);
	// end CATEGORIES cleanup
	// ------------------------------------------------------------------------------------- //

	// ------------------------------------------------------------------------------------- //
	// begin SoGo fixes (company vCards without N and FN attributes)
	//  we must perform vCard fixes here because the N and FN attributes are used in the collection list

	// if N attribute is missing we add it
	if(vcardString.match(vCard.pre['contentline_N'])==null)
		tmp.splice(1,0,'N:;;;;');

	// if FN attribute is missing we add it
	if(vcardString.match(vCard.pre['contentline_FN'])==null)
	{
		var fn_value='';
		var tmp2=null;
		// if there is an ORG attribute defined, we use the company name as fn_value (instead of empty string)
		if((tmp2=vcardString.match(vCard.pre['contentline_ORG']))!=null)
		{
			// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
			var parsed=tmp2[0].match(vCard.pre['contentline_parse']);
			// parsed_value = [0]->Org, [1..]->Org Units
			var parsed_value=vcardSplitValue(parsed[4],';');
			fn_value=parsed_value[0];
		}
		tmp.splice(1,0,'FN:'+fn_value);
	}
	vcardString=tmp.join('\r\n');
	// end SoGo fixes
	// ------------------------------------------------------------------------------------- //

	return {vcard: vcardString, categories: categoriesTxt};
}

function additionalRFCFixes(vcardString)
{
	// ------------------------------------------------------------------------------------- //
	var tmp=vcardString.split('\r\n');

	// update non-RFC attributes (special transformations)
	for(var i=1;i<tmp.length-2;i++)
	{
		// parsed = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
		var parsed=('\r\n'+tmp[i]+'\r\n').match(vCard.pre['contentline_parse']);

		if(parsed!=null)
		{
			switch(parsed[2])
			{
				case 'TEL':
					// remove the non-RFC params (Evolution bug)
					var parsed_value=vcardSplitParam(parsed[3]);
					for(var j=parsed_value.length-1;j>0;j--)
						if(parsed_value[j].match(vCard.pre['additionalRFCFixes_tel-param'])==null)
							parsed_value.splice(j,1);

					parsed[3]=parsed_value.join(';');
					tmp[i]=parsed[1]+parsed[2]+parsed[3]+':'+parsed[4];
					break;
				case 'EMAIL':
					// transform the params separated by ',' to 'TYPE=' params and remove the non-RFC params (Evolution bug)
					var parsed_value=vcardSplitParam(parsed[3]);
					for(var j=parsed_value.length-1;j>0;j--)
						if(parsed_value[j].match(vCard.pre['additionalRFCFixes_email-param'])==null)
						{
							if((transformed=parsed_value[j].replace(vCard.pre['additionalRFCFixes_comma-g'], ';TYPE=')).match(vCard.pre['additionalRFCFixes_email-params'])!=null)
								parsed_value[j]=transformed;
							else
								parsed_value.splice(j,1);
						}

					parsed[3]=parsed_value.join(';');
					// add missing and required "internet" type (Sogo bug)
					if(parsed[3].match(vCard.pre['additionalRFCFixes_type-internet'])==null)
						parsed[3]+=';TYPE=INTERNET';

					tmp[i]=parsed[1]+parsed[2]+parsed[3]+':'+parsed[4];
					break;
// the upcoming vCard 4.0 allows params for URL and many clients use it also in vCard 3.0
//				case 'URL':	// no params allowed for URL (Evolution bug)
//					tmp[i]=parsed[1]+parsed[2]+':'+parsed[4];
//					break;
				default:
					break;
			}
		}
	}
	vcardString=tmp.join('\r\n');
	// ------------------------------------------------------------------------------------- //

	return vcardString;
}

// transform the vCard to the editor expected format
function normalizeVcard(vcardString)
{
	var parsed=null;
	// remove the PRODID element (unusable for the editor)
	while((parsed=vcardString.match(vCard.pre['contentline_PRODID']))!=null)
		vcardString=vcardString.replace(parsed[0],'\r\n');

	var tmp=vcardString.split('\r\n');
	var vcard_begin=tmp[0].replace(vCard.pre['normalizeVcard_group_w_dot'], 'item.')+'\r\n';
	var vcard_end=tmp[tmp.length-2].replace(vCard.pre['normalizeVcard_group_w_dot'], 'item.')+'\r\n';
	// remove the vCard BEGIN and END and all duplicate entries (usually created by other buggy clients)
	vcardString='\r\n'+tmp.slice(1, tmp.length-2).join('\r\n')+'\r\n';

	var vcard_out_grouped=new Array();
	while((parsed=vcardString.match(vCard.pre['contentline_parse']))!=null)
	{
		var additional_related='';
		var vcard_element_related='';
		var attr_name='';
		var params_swc='';
		var attr_value='';

		// parsed = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
		var params_array=vcardSplitParam(parsed[3]);
		// we transform the old X-* IM attributes to new IMPP (internally used by editor)
		switch(parsed[2])
		{
			case 'X-ABDATE':
				attr_name=parsed[2];
				params_swc=params_array.sort().join(';').toUpperCase();	// we need upper case here to remove duplicate values later
				tmp=parsed[4].match(vCard.pre['normalizeVcard_date']);
				attr_value=tmp[1]+'-'+tmp[2]+'-'+tmp[3];	// sorry, we support only date (no date-time support)
				break;
			case 'X-EVOLUTION-ANNIVERSARY':
			case 'X-ANNIVERSARY':
				attr_name='X-ABDATE';
				params_swc='';
				tmp=parsed[4].match(vCard.pre['normalizeVcard_date']);
				attr_value=tmp[1]+'-'+tmp[2]+'-'+tmp[3];	// sorry, we support only date (no date-time support)
				additional_related='X-ABLabel:_$!<Anniversary>!$_\r\n';

				// check for X-ABDATE attribute with the same value
				var found=false;
				var tmpVcardString=vcardString;
				var tmp_vcard_element=null;
				while((tmp_vcard_element=tmpVcardString.match(vCard.pre['contentline_X-ABDATE']))!=null)
				{
					// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
					var tmp_parsed=tmp_vcard_element[0].match(vCard.pre['contentline_parse']);

					if(tmp_parsed[4]==parsed[4] || tmp_parsed[4]==attr_value)
					{
						found=true;
						break;
					}
					tmpVcardString=tmpVcardString.replace(tmp_vcard_element[0], '\r\n');
				}

				if(found==true)
				{
					// remove the processed element
					vcardString=vcardString.replace(parsed[0], '\r\n');
					// find the corresponding group data (if exists)
					if(parsed[1]!='')
					{
						var re=RegExp('\r\n'+parsed[1].replace('.', '\\..*')+'\r\n', 'm');
						while((vcard_element_related=vcardString.match(re))!=null)
							vcardString=vcardString.replace(vcard_element_related[0], '\r\n');	// remove the processed parameter
					}
					continue;
				}
				break;
			case 'BDAY':
				attr_name=parsed[2];
				params_swc=';VALUE=date';
				tmp=parsed[4].match(vCard.pre['normalizeVcard_date']);
				attr_value=tmp[1]+'-'+tmp[2]+'-'+tmp[3];	// sorry, we support only date (no date-time support)
				break;
			case 'X-AIM':
			case 'X-JABBER':
			case 'X-MSN':
			case 'X-YAHOO':
			case 'X-YAHOO-ID':
			case 'X-ICQ':
			case 'X-SKYPE':
				attr_name='IMPP';
				if(params_array.length==0)
					params_array[0]='';	// after the join it generates ';' after the attribute name
				params_array[params_array.length]='X-SERVICE-TYPE='+parsed[2].replace(vCard.pre['normalizeVcard_xb_or_ide'], '');	// extract the IM type
				params_swc=params_array.sort().join(';');
				attr_value=parsed[4];

				// check for IMPP attribute with the same value
				var found=false;
				var tmpVcardString=vcardString;
				var tmp_vcard_element=null;
				while((tmp_vcard_element=tmpVcardString.match(vCard.pre['contentline_IMPP']))!=null)
				{
					// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
					var tmp_parsed=tmp_vcard_element[0].match(vCard.pre['contentline_parse']);

					if(tmp_parsed[4].replace(vCard.pre['normalizeVcard_before_val'], '')==parsed[4])
					{
						found=true;
						break;
					}
					tmpVcardString=tmpVcardString.replace(tmp_vcard_element[0], '\r\n');
				}

				if(found==true)
				{
					// remove the processed element
					vcardString=vcardString.replace(parsed[0], '\r\n');
					// find the corresponding group data (if exists)
					if(parsed[1]!='')
					{
						var re=RegExp('\r\n'+parsed[1].replace('.', '\\..*')+'\r\n', 'm');
						while((vcard_element_related=vcardString.match(re))!=null)
							vcardString=vcardString.replace(vcard_element_related[0], '\r\n');	// remove the processed parameter
					}
					continue;
				}
				break;
			case 'IMPP':
				attr_name=parsed[2];
				params_swc=params_array.sort().join(';').toUpperCase();	// we need upper case here to remove duplicate values later

				// remove the '*:' from the '*:value'
				//  but we add them back during the vcard generation from the interface
				attr_value=vcardSplitValue(parsed[4], ':').splice(1, 1).join('')
				break;
			case 'X-ASSISTANT':
			case 'X-EVOLUTION-ASSISTANT':
				attr_name='X-ABRELATEDNAMES';
				params_swc='';
				attr_value=parsed[4];
				additional_related='X-ABLabel:_$!<Assistant>!$_\r\n';

				// check for X-ABRELATEDNAMES attribute with the same value
				var found=false;
				var tmpVcardString=vcardString;
				var tmp_vcard_element=null;
				while((tmp_vcard_element=tmpVcardString.match(vCard.pre['contentline_X-ABRELATEDNAMES']))!=null)
				{
					// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
					var tmp_parsed=tmp_vcard_element[0].match(vCard.pre['contentline_parse']);

					if(tmp_parsed[4]==parsed[4])
					{
						found=true;
						break;
					}
					tmpVcardString=tmpVcardString.replace(tmp_vcard_element[0], '\r\n');
				}

				if(found==true)
				{
					// remove the processed element
					vcardString=vcardString.replace(parsed[0], '\r\n');
					// find the corresponding group data (if exists)
					if(parsed[1]!='')
					{
						var re=RegExp('\r\n'+parsed[1].replace('.', '\\..*')+'\r\n', 'm');
						while((vcard_element_related=vcardString.match(re))!=null)
							vcardString=vcardString.replace(vcard_element_related[0], '\r\n');	// remove the processed parameter
					}
					continue;
				}
				break;
			case 'X-MANAGER':
			case 'X-EVOLUTION-MANAGER':
				attr_name='X-ABRELATEDNAMES';
				params_swc='';
				attr_value=parsed[4];
				additional_related='X-ABLabel:_$!<Manager>!$_\r\n';

				// check for X-ABRELATEDNAMES attribute with the same value
				var found=false;
				var tmpVcardString=vcardString;
				var tmp_vcard_element=null;
				while((tmp_vcard_element=tmpVcardString.match(vCard.pre['contentline_X-ABRELATEDNAMES']))!=null)
				{
					// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
					var tmp_parsed=tmp_vcard_element[0].match(vCard.pre['contentline_parse']);

					if(tmp_parsed[4]==parsed[4])
					{
						found=true;
						break;
					}
					tmpVcardString=tmpVcardString.replace(tmp_vcard_element[0], '\r\n');
				}

				if(found==true)
				{
					// remove the processed element
					vcardString=vcardString.replace(parsed[0], '\r\n');
					// find the corresponding group data (if exists)
					if(parsed[1]!='')
					{
						var re=RegExp('\r\n'+parsed[1].replace('.', '\\..*')+'\r\n', 'm');
						while((vcard_element_related=vcardString.match(re))!=null)
							vcardString=vcardString.replace(vcard_element_related[0], '\r\n');	// remove the processed parameter
					}
					continue;
				}
				break;
			case 'X-SPOUSE':
			case 'X-EVOLUTION-SPOUSE':
				attr_name='X-ABRELATEDNAMES';
				params_swc='';
				attr_value=parsed[4];
				additional_related='X-ABLabel:_$!<Spouse>!$_\r\n';

				// check for X-ABRELATEDNAMES attribute with the same value
				var found=false;
				var tmpVcardString=vcardString;
				var tmp_vcard_element=null;
				while((tmp_vcard_element=tmpVcardString.match(vCard.pre['contentline_X-ABRELATEDNAMES']))!=null)
				{
					// parsed (contentline_parse) = [1]->"group.", [2]->"name", [3]->";param;param", [4]->"value"
					var tmp_parsed=tmp_vcard_element[0].match(vCard.pre['contentline_parse']);

					if(tmp_parsed[4]==parsed[4])
					{
						found=true;
						break;
					}
					tmpVcardString=tmpVcardString.replace(tmp_vcard_element[0], '\r\n');
				}

				if(found==true)
				{
					// remove the processed element
					vcardString=vcardString.replace(parsed[0], '\r\n');
					// find the corresponding group data (if exists)
					if(parsed[1]!='')
					{
						var re=RegExp('\r\n'+parsed[1].replace('.', '\\..*')+'\r\n', 'm');
						while((vcard_element_related=vcardString.match(re))!=null)
							vcardString=vcardString.replace(vcard_element_related[0], '\r\n');	// remove the processed parameter
					}
					continue;
				}
				break;
			default:
				attr_name=parsed[2];
				params_swc=params_array.sort().join(';');
				attr_value=parsed[4];
				break;
		}
		// remove the processed element
		vcardString=vcardString.replace(parsed[0],'\r\n');

		if(attr_name!='FN' && attr_name!='N' && attr_value=='')	// attributes with empty values are not supported and are removed here
		{
			// find the corresponding group data (if exists)
			if(parsed[1]!='')
			{
				var re=RegExp('\r\n'+parsed[1].replace('.','\\..*')+'\r\n', 'm');
				while((vcard_element_related=vcardString.match(re))!=null)
					// remove the processed parameter
					vcardString=vcardString.replace(vcard_element_related[0], '\r\n');
			}
			continue;
		}

		// add the new element to output array (without group)
		grouped_elem=new Array();
		grouped_elem[grouped_elem.length]=attr_name+params_swc+':'+attr_value+'\r\n';
		if(additional_related!='')	// used if we manually add related items as a part of transformation
			grouped_elem[grouped_elem.length]=additional_related;
		// find the corresponding group data (if exists)
		if(parsed[1]!='')
		{
			var re=RegExp('\r\n'+parsed[1].replace('.','\\.(.*)')+'\r\n', 'm');
			while((vcard_element_related=vcardString.match(re))!=null)
			{
				// add the related element to array
				grouped_elem[grouped_elem.length]=vcard_element_related[1]+'\r\n';
				// remove the processed parameter
				vcardString=vcardString.replace(vcard_element_related[0], '\r\n');
			}
		}
		// add the new grouped element to output
		vcard_out_grouped[vcard_out_grouped.length]=grouped_elem.sort().join('');
	}
//
// after the transformation and grouping we remove all identical elements and preserve sorting
	//  (for example X-AIM and IMPP;X-SERVICE-TYPE=AIM, ...)
	for(var i=vcard_out_grouped.length-1;i>=0;i--)
		if(vcard_out_grouped.slice(0,i).indexOf(vcard_out_grouped[i])!=-1)
			vcard_out_grouped.splice(i,1);

	// add new group names ...
	elemCounter=0;
	for(i=0;i<vcard_out_grouped.length;i++)
		if(vcard_out_grouped[i].match(vCard.pre['normalizeVcard_rn-gm']).length>1)
			vcard_out_grouped[i]=(('\r\n'+vcard_out_grouped[i].substring(0, vcard_out_grouped[i].length-2)).replace(vCard.pre['normalizeVcard_rn-gm'], '\r\nitem'+(elemCounter++)+'.')+'\r\n').substring(2);

	vcard_out_grouped.unshift(vcard_begin);
	vcard_out_grouped.push(vcard_end);

	return vcard_out_grouped.join('');
}
