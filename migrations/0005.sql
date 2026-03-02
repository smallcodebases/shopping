CREATE TABLE item_stores_2 (
  item INTEGER NOT NULL REFERENCES items (id) ON DELETE CASCADE,
  store INTEGER NOT NULL REFERENCES stores (id) ON DELETE CASCADE,
  sold INTEGER NOT NULL CHECK (sold IN (0, 1)),
  location INTEGER REFERENCES locations (id) ON DELETE SET NULL,
  PRIMARY KEY (item, store)
) WITHOUT ROWID;

INSERT INTO item_stores_2 (item, store, sold, location)
SELECT item, store, true, location
FROM item_stores
WHERE location IS NOT NULL;

INSERT INTO item_stores_2 (item, store, sold, location)
SELECT item, store, false, NULL
FROM item_stores
WHERE location IS NULL;

DROP TABLE item_stores;

ALTER TABLE item_stores_2 RENAME TO item_stores;
