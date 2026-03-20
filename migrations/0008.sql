ALTER TABLE locations RENAME TO sections;

ALTER TABLE item_stores RENAME COLUMN location TO section;

DROP INDEX locations_store_id_unique;
CREATE UNIQUE INDEX sections_store_id_unique ON sections (store, id);
