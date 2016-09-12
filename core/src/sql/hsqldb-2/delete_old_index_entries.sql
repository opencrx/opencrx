select count(*) from 
  oocke1_indexentry i1 
where 
  i1.modified_at < (select max(modified_at) from oocke1_indexentry i2 where i1.indexed_object = i2.indexed_object);

delete from 
  oocke1_indexentry i1 
where 
  i1.modified_at < (select max(modified_at) from oocke1_indexentry i2 where i1.indexed_object = i2.indexed_object);

delete from
  oocke1_indexentry_ i_
where 
  not exists (select 0 from oocke1_indexentry i where i_.object_id = i.object_id);
 
