-- A UNIQUE (store, position) on this table would be more correct, but makes reordering annoying (have to temporarily
-- assign negative positions or something).

-- A UNIQUE (store, name) on this table probably makes sense in most cases, but I can imagine some store with, like,
-- two "Deli" or something. IDK.

CREATE TABLE locations_2 (
  id INTEGER PRIMARY KEY,
  store INTEGER NOT NULL REFERENCES stores (id) ON DELETE CASCADE,
  position INTEGER NOT NULL,
  name TEXT NOT NULL
);

INSERT INTO locations_2 (id, store, position, name)
SELECT id, store, position, name
FROM locations;

DROP TABLE locations;

ALTER TABLE locations_2 RENAME TO locations;

-- This is necessary for foreign keys from item_stores. Obviously locations.id itself is unique.
CREATE UNIQUE INDEX locations_store_id_unique ON locations (store, id);
