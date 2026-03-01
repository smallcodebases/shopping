DROP TABLE locations;

CREATE TABLE locations (
  id INTEGER PRIMARY KEY,
  store INTEGER NOT NULL REFERENCES stores (id) ON DELETE CASCADE,
  position INTEGER NOT NULL,
  name TEXT NOT NULL,
  UNIQUE (store, id),
  UNIQUE (store, position),
  UNIQUE (store, name)
);

CREATE TABLE item_stores_2 (
  item INTEGER NOT NULL REFERENCES items (id) ON DELETE CASCADE,
  store INTEGER NOT NULL REFERENCES stores (id) ON DELETE CASCADE,
  location INTEGER,
  PRIMARY KEY (item, store),
  FOREIGN KEY (store, location) REFERENCES locations (store, id) ON DELETE CASCADE
) WITHOUT ROWID;

INSERT INTO item_stores_2 (item, store, location)
SELECT item, store, location
FROM item_stores
WHERE in_store = 1;

INSERT INTO item_stores_2 (item, store, location)
SELECT item, store, NULL
FROM item_stores
WHERE in_store = 0;

DROP TABLE item_stores;

ALTER TABLE item_stores_2
RENAME TO item_stores;
